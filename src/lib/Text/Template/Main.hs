module Text.Template.Main where

import Text.Template.Flags
import Text.Template.Input.Json
import Text.Template.Input.Stdin
import Text.Template.Interpreter
import Text.Template.Parser

import Control.Monad.Except

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
    let tmplFile = inputFile args
        luaFile  = replaceExtension (inputFile args) "lua"
        varFile  = replaceExtension (inputFile args) "var"
    bs  <- BL.readFile tmplFile
    lua <- BS.readFile luaFile
    var <- BL.readFile varFile
    json <- case inputValues args of
        Nothing -> pure Nothing
        Just fp -> Just <$> BL.readFile fp
    resultOrErr <- runTemplate tmplFile varFile bs lua var json
    result <- case resultOrErr of
        Left err     -> reportError err
        Right result -> pure result
    case outputFile args of
        Nothing -> BLC.putStrLn result
        Just fp -> do
            exists <- doesFileExist fp
            case (exists, overwriteOutput args) of
                (True, False) -> reportError $ fp <> " exists and --overwrite was not specified."
                _             -> BL.writeFile fp result

runTemplate :: FilePath -> FilePath -> BL.ByteString -> BS.ByteString -> BL.ByteString -> Maybe BL.ByteString -> IO (Either String BL.ByteString)
runTemplate tmplFile varFile bs lua var mJson = runExceptT $ do
    stmts <- ExceptT $ pure $ parseTemplate tmplFile bs
    pat   <- ExceptT $ pure $ parsePatternDecls varFile var
    vars <- case mJson of
        Nothing   -> liftIO $ getInputs pat
        Just json -> ExceptT $ pure $ jsonToInputs json pat
    ExceptT $ do
        result <- interpret stmts lua vars
        pure $ case result of
            Left err -> Left $ fromInterpreterException err
            Right rs -> Right rs

reportError :: String -> IO a
reportError err = do
    hPutStrLn stderr err
    exitFailure
