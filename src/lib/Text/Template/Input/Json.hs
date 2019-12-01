module Text.Template.Input.Json
    ( jsonToInputs
    ) where

import Text.Template.Input.Stdin (showType)
import Text.Template.Interpreter
import Text.Template.Parser

import Control.Monad

import qualified Data.Aeson as A
import Data.Scientific (toRealFloat)
import qualified Data.HashMap.Strict as HM

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import Data.Text.Encoding as T

import Data.Foldable

jsonToInputs :: BL.ByteString -> [PatternDecl] -> Either String Variables
jsonToInputs jsonText decls = do
    value <- A.eitherDecode' jsonText
    Variables <$> mapM (valueToDecl value) decls

valueToDecl :: A.Value -> PatternDecl -> Either String (String, Value)
valueToDecl value (PatternDecl name typ) = case value of
    A.Object m -> case HM.lookup (T.decodeUtf8 $ BL.toStrict name) m of
        Nothing -> Left $ "Key not found in top level object: " <> BLC.unpack name
        Just v' -> amendError name $ (,) (BLC.unpack name) <$> valueToInput typ v'
    _          -> Left "Top level value must be an object"

valueToInput :: PatternType -> A.Value -> Either String Value
valueToInput typ value = case typ of
    StringType -> case value of
        A.String t -> pure $ StringValue $ T.encodeUtf8 t
        _          -> typeMismatch value typ
    NumberType -> case value of
        A.Number n -> pure $ NumberValue $ toRealFloat n
        _          -> typeMismatch value typ
    BoolType -> case value of
        A.Bool b -> pure $ BoolValue b
        _          -> typeMismatch value typ
    ListType t -> case value of
        A.Array v -> amendError (BLC.pack "list") $ ListValue <$> mapM (valueToInput t) (toList v)
        _         -> typeMismatch value typ
    TupleType xs -> case value of
        A.Array v -> case toList v of
            ys -> if length ys >= length xs
                then ListValue <$> zipWithM valueToInput xs ys
                else Left $ "Expected at least " <> show (length xs) <> " array elements, found only " <> show (length ys) <> " elements"
        _         -> typeMismatch value typ
    MapType xs -> case value of
        A.Object m -> do
            ys <- forM xs $ \ (MapPattern k v) -> case HM.lookup (T.decodeUtf8 $ BL.toStrict k) m of
                Nothing -> Left $ "Key not found in object: " <> BLC.unpack k
                Just v' -> amendError k $ (,) (StringValue $ BL.toStrict k) <$> valueToInput v v'
            pure $ MapValue ys
        _          -> typeMismatch value typ

typeMismatch :: A.Value -> PatternType -> Either String a
typeMismatch value typ = Left $ "Expected " <> showType typ <> ", got " <> showJsonType value

showJsonType :: A.Value -> String
showJsonType value = case value of
    A.Object{} -> "object"
    A.Array{}  -> "array"
    A.String{} -> "string"
    A.Number{} -> "number"
    A.Bool{}   -> "bool"
    A.Null     -> "null"

amendError :: BL.ByteString -> Either String a -> Either String a
amendError name m = case m of
    Right vl -> Right vl
    Left err -> Left $ "In " <> BLC.unpack name <> ": " <> err