module Text.Template.Main where

import Text.Template.Flags
import Text.Template.Input.Json
import Text.Template.Input.Stdin
import Text.Template.Interpreter
import Text.Template.Parser

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import System.Exit
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension)
import System.IO

libMain :: IO ()
libMain = do
    args <- parseFlags
    execMain args

execMain :: Flags -> IO ()
execMain args = do
    bs  <- BL.readFile $ inputFile args
    lua <- BS.readFile $ replaceExtension (inputFile args) "lua"
    var <- BL.readFile $ replaceExtension (inputFile args) "var"
    case (,) <$> parseTemplate bs <*> parsePatternDecls var of
        Left err -> reportError err
        Right (xs, pat) -> do
            vars <- case inputValues args of
                Nothing -> getInputs pat
                Just fp -> do
                    json <- BL.readFile fp
                    case jsonToInputs json pat of
                        Left err   -> reportError err
                        Right vars -> pure vars
            resultOrErr <- interpret xs lua vars
            result <- case resultOrErr of
                Left err -> reportError $ fromInterpreterException err
                Right rs -> pure rs
            case outputFile args of
                Nothing -> BLC.putStrLn result
                Just fp -> do
                    exists <- doesFileExist fp
                    case (exists, overwriteOutput args) of
                        (True, False) -> reportError $ fp <> " exists and --overwrite was not specified."
                        _             -> BL.writeFile fp result

reportError :: String -> IO a
reportError err = do
    hPutStrLn stderr err
    exitFailure
