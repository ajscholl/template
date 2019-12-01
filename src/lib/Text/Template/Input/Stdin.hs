module Text.Template.Input.Stdin
    ( getInputs
    , showType
    ) where

import Text.Template.Interpreter
import Text.Template.Parser

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import Data.List
import Data.Maybe

import System.Console.Haskeline
import System.Directory (createDirectoryIfMissing, getUserDocumentsDirectory)
import System.FilePath

import Text.Read
import Text.Printf

getInputs :: [PatternDecl] -> IO Variables
getInputs decls = do
    documentsDir <- getUserDocumentsDirectory
    let historyFileDir = documentsDir </> "Template"
        historyFileName = historyFileDir </> ".history"
    createDirectoryIfMissing True historyFileDir
    runInputT defaultSettings { historyFile = Just historyFileName } $ Variables <$> mapM (getInput []) decls

getInput :: [String] -> PatternDecl -> InputT IO (String, Value)
getInput path (PatternDecl name typ) = do
    value <- getInputWithType (path ++ [string name]) typ
    pure (string name, value)

getInputWithType :: [String] -> PatternType -> InputT IO Value
getInputWithType path typ = case typ of
    StringType -> do
        input <- getInputLine $ "Please provide input for the variable " <> pathString path <> " :: " <> showType typ <> ": "
        when (isNothing input) $ liftIO $ fail "EOF"
        pure $ StringValue $ BSC.pack $ fromMaybe "" input
    NumberType -> do
        input <- getInputLine $ "Please provide input for the variable " <> pathString path <> " :: " <> showType typ <> ": "
        when (isNothing input) $ liftIO $ fail "EOF"
        case input >>= readMaybe of
            Just number -> pure $ NumberValue number
            Nothing     -> do
                outputStrLn "Invalid value, please enter a number"
                getInputWithType path typ
    BoolType -> do
        input <- getInputLine $ "Please provide input for the variable " <> pathString path <> " :: " <> showType typ <> ": "
        case input of
            Just "true"  -> pure $ BoolValue True
            Just "false" -> pure $ BoolValue False
            Nothing      -> liftIO $ fail "EOF"
            _ -> do
                outputStrLn "Invalid value, please enter \"true\" or \"false\""
                getInputWithType path typ
    ListType t -> do
        input <- getInputLine $ "Please provide the number of inputs for the variable " <> pathString path <> " :: " <> showType typ <> ": "
        when (isNothing input) $ liftIO $ fail "EOF"
        case input >>= readMaybe of
            Nothing     -> do
                outputStrLn "Invalid value, please enter a number"
                getInputWithType path typ
            Just count -> ListValue <$> mapM (\ i -> getInputWithType (path ++ [show i]) t) [0 :: Int .. count - 1]
    TupleType xs -> ListValue <$> zipWithM (\ i -> getInputWithType (path ++ [show i])) [0 :: Int ..] xs
    MapType xs -> MapValue <$> mapM (\ (MapPattern k v) -> (,) (StringValue $ BL.toStrict k) <$> getInputWithType (path ++ [string k]) v) xs

pathString :: [String] -> String
pathString = intercalate "."

string :: BL.ByteString -> String
string = BLC.unpack

showType :: PatternType -> String
showType t = case t of
    StringType -> "String"
    NumberType -> "Number"
    BoolType   -> "Bool"
    ListType e -> printf "[%s]" $ showType e
    TupleType xs -> printf "(%s)" $ intercalate ", " $ map showType xs
    MapType xs -> printf "(%s)" $ intercalate ", " $ map showMapPattern xs

showMapPattern :: MapPattern -> String
showMapPattern (MapPattern k v) = string k <> ": " <> showType v
