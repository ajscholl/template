module Text.Template.Main where

import Text.Template.Input.Json
import Text.Template.Input.Stdin
import Text.Template.Interpreter
import Text.Template.Parser

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import System.Environment

libMain :: IO ()
libMain = do
    args <- getArgs
    case args of
        [arg] -> do
            bs  <- BL.readFile $ arg <> ".tmpl"
            lua <- BS.readFile $ arg <> ".lua"
            var <- BL.readFile $ arg <> ".var"
            case (,) <$> parseTemplate bs <*> parsePatternDecls var of
                Left err -> print err
                Right (xs, pat) -> do
                    vars <- getInputs pat
                    result <- interpret xs lua vars
                    BLC.putStrLn result
        [arg, jsonArg] -> do
            bs  <- BL.readFile $ arg <> ".tmpl"
            lua <- BS.readFile $ arg <> ".lua"
            var <- BL.readFile $ arg <> ".var"
            json <- BL.readFile $ jsonArg
            case (,) <$> parseTemplate bs <*> parsePatternDecls var of
                Left err -> print err
                Right (xs, pat) -> do
                    case jsonToInputs json pat of
                        Left err -> print err
                        Right vars -> do
                            result <- interpret xs lua vars
                            BLC.putStrLn result
