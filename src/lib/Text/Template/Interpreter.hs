{-# LANGUAGE DeriveDataTypeable #-}
module Text.Template.Interpreter where

import Control.Exception
import Control.Monad.Writer

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import Data.List
import Data.Maybe
import Data.String
import Data.Typeable

import Foreign.Lua hiding (Exception)

import Text.Template.Parser

newtype Variables = Variables [(String, Value)]

data Value = StringValue !ByteString
           | NumberValue !Double
           | BoolValue !Bool
           | ListValue ![Value]
           | MapValue ![(Value, Value)]
           deriving Show

newtype InterpreterException = InterpreterException String deriving (Show, Typeable)

instance Exception InterpreterException

interpret :: [Stmt] -> ByteString -> Variables -> IO BL.ByteString
interpret stmts luaCode vars = run $ do
    -- load lua libraries
    forM_ [("base", openbase), ("math", openmath), ("string", openstring), ("table", opentable)] $ \ (name, act) -> do
        act
        setglobal name
        checkStatus
    -- load lua code
    s <- dostring luaCode
    case s of
        OK        -> pure ()
        ErrSyntax -> liftIO $ throwIO $ InterpreterException "Failed to parse lua code: Syntax error"
        _         -> liftIO $ throwIO $ InterpreterException $ "Failed to load lua code: " <> show s
    setVariables vars
    execWriterT $ mapM_ (runStmt vars) stmts

checkStatus :: Lua ()
checkStatus = do
    s <- status
    case s of
        OK -> pure ()
        _  -> liftIO $ throwIO $ InterpreterException $ "Lua has invalid status: " <> show s

runStmt :: Variables -> Stmt -> WriterT BL.ByteString Lua ()
runStmt vars stmt = case stmt of
    VerbatimStmt bl -> tell bl
    PrintStmt (Expr expr) -> do
        value <- lift $ evaluateExpr expr
        tell $ valueToString value
    IfStmt (Expr expr) ifBlock elseIfBlocks elseBlock -> do
        value <- lift $ evaluateExpr expr
        case value of
            BoolValue True -> mapM_ (runStmt vars) ifBlock
            BoolValue False -> case elseIfBlocks of
                [] -> mapM_ (runStmt vars) elseBlock
                ((e, b):xs) -> runStmt vars $ IfStmt e b xs elseBlock
            _ -> liftIO $ throwIO $ InterpreterException "Received non-boolean value in if-condition"
    ForStmt (pattern, Expr expr) body -> do
        value <- lift $ evaluateExpr expr
        case value of
            ListValue xs -> forM_ xs $ \ v -> do
                newVars <- lift $ matchPattern pattern v vars
                lift $ setVariables newVars
                mapM_ (runStmt newVars) body
            MapValue xs  -> forM_ xs $ \ (k, v) -> do
                newVars <- lift $ matchPatternWithKey pattern k v vars
                lift $ setVariables newVars
                mapM_ (runStmt newVars) body
            _ -> liftIO $ throwIO $ InterpreterException "Received non-list value in for-loop"
        lift $ setVariables vars

resultGlobal :: IsString a => a
resultGlobal = fromString "__result_global"

evaluateExpr :: BL.ByteString -> Lua Value
evaluateExpr expr = do
    s <- dostring $ BL.toStrict $ resultGlobal <> fromString " = " <> expr
    case s of
        OK        -> pure ()
        ErrSyntax -> liftIO $ throwIO $ InterpreterException $ "Failed to parse lua expression: Syntax error in " <> show expr
        _         -> liftIO $ throwIO $ InterpreterException $ "Failed to evaluate lua expression: " <> show s <> " in " <> show expr
    getglobal resultGlobal
    value <- parseValue (-1)
    pop 1
    pure value

-- parse the value at the given index to a haskell value and leave the stack unmodified
parseValue :: StackIndex -> Lua Value
parseValue idx = do
    typ <- ltype idx
    case typ of
        TypeNone          -> liftIO $ throwIO $ InterpreterException "Can not convert none to value"
        TypeNil           -> liftIO $ throwIO $ InterpreterException "Can not convert nil to value"
        TypeBoolean       -> BoolValue <$> toboolean idx
        TypeLightUserdata -> liftIO $ throwIO $ InterpreterException "Can not convert light userdata to value"
        TypeNumber        -> NumberValue . maybe 0 (\ (Number d) -> d) <$> tonumber idx
        TypeString        -> StringValue . fromMaybe mempty <$> tostring idx
        TypeTable         -> parseTable idx
        TypeFunction      -> liftIO $ throwIO $ InterpreterException "Can not convert function to value"
        TypeUserdata      -> liftIO $ throwIO $ InterpreterException "Can not convert userdata to value"
        TypeThread        -> liftIO $ throwIO $ InterpreterException "Can not convert thread to value"

-- convert a table at the given stack
parseTable :: StackIndex -> Lua Value
parseTable idx = do
    -- stack empty
    pushvalue idx
    -- stack: -1 -> table
    pushnil
    -- stack: -1 -> nil, -2 -> table
    let go = do
            hasNext <- next (-2)
            if hasNext
            then do
                -- stack: -1 value, -2 key, -3 table
                k <- parseValue (-2)
                v <- parseValue (-1)
                pop 1
                -- stack: -1 key, -2 table
                xs <- go
                pure $ (k, v) : xs
            else pure []
    xs <- go
    pop 1
    pure $ case mkListValue xs of
        Just ys -> ListValue ys
        Nothing -> MapValue xs

mkListValue :: [(Value, Value)] -> Maybe [Value]
mkListValue xs = do
    let checkIndex (NumberValue n, v) = do
            let i = floor n
            guard $ fromIntegral i == n
            pure (i, v)
        checkIndex _ = Nothing
        checkNoGaps [] _ = pure []
        checkNoGaps ((xi, xv):ys) n = do
            guard $ n == (xi :: Int)
            zs <- checkNoGaps ys $! n + 1
            pure $ xv : zs
    ys <- sortOn fst <$> mapM checkIndex xs
    checkNoGaps ys 1

valueToString :: Value -> BL.ByteString
valueToString val = case val of
    StringValue bs -> BL.fromStrict bs
    NumberValue n  -> fromString $ show n
    BoolValue b    -> if b then fromString "true" else fromString "value"
    ListValue xs   -> mconcat $ intersperse (fromString ",") $ map valueToString xs
    MapValue xs    -> mconcat $ intersperse (fromString ",") $ map (\ (k, v) -> fromString "{" <> valueToString k <> fromString "," <> valueToString v <> fromString "}") xs

setVariables :: Variables -> Lua ()
setVariables (Variables xs) = mapM_ setVariable xs

setVariable :: (String, Value) -> Lua ()
setVariable (name, value) = do
    pushValue value
    setglobal name
    checkStatus
    where
        pushValue var = case var of
            StringValue bs -> pushstring bs
            NumberValue n  -> pushnumber $ Number n
            BoolValue b    -> pushboolean b
            ListValue xs   -> do
                createtable (length xs) 0
                forM_ (zip [1..] xs) $ \ (i, v) -> do
                    pushnumber $ Number i
                    pushValue v
                    settable (-3)
            MapValue xs    -> do
                createtable 0 (length xs)
                forM_ xs $ \ (k, v) -> do
                    pushValue k
                    pushValue v
                    settable (-3)

matchPattern :: Pattern -> Value -> Variables -> Lua Variables
matchPattern pat values vars@(Variables variables) = case pat of
    WildP     -> pure vars
    VarP name -> pure (Variables $ variables ++ [(BLC.unpack name, values)])
    TupP pats -> case pats of
        []  -> pure vars
        [x] -> matchPattern x values vars
        _   -> case values of
            ListValue xs -> if length xs >= length pats
                then foldl' (>>=) (pure vars) $ zipWith matchPattern pats xs
                else liftIO $ throwIO $ InterpreterException "Failed pattern match"
            _ -> liftIO $ throwIO $ InterpreterException "Can only match tuples to lists"

matchPatternWithKey :: Pattern -> Value -> Value -> Variables -> Lua Variables
matchPatternWithKey pat key values = case values of
    StringValue{} -> matchPattern pat (ListValue [key, values])
    NumberValue{} -> matchPattern pat (ListValue [key, values])
    BoolValue{}   -> matchPattern pat (ListValue [key, values])
    ListValue xs  -> matchPattern pat (ListValue $ key : xs)
    MapValue{}    -> matchPattern pat (ListValue [key, values])
