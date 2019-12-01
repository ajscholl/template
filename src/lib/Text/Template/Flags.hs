module Text.Template.Flags (Flags(..), parseFlags) where

import Options.Applicative

data Flags = Flags
    { inputFile       :: FilePath
    , inputValues     :: Maybe FilePath
    , outputFile      :: Maybe FilePath
    , overwriteOutput :: Bool
    } deriving Show

flags :: Parser Flags
flags = Flags
    <$> strOption (long "input" <> short 'i' <> metavar "TEMPLATE" <> help "The input template.")
    <*> optional (strOption (long "values" <> short 'v' <> metavar "JSON-FILE" <> help "File containing values to fill into the template."))
    <*> optional (strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Where to write the output to."))
    <*> switch (long "overwrite" <> short 'f' <> help "Overwrite output file if it already exists.")

infoFlags :: ParserInfo Flags
infoFlags = info flags $ failureCode 1
    <> progDesc "A small tool to expand templates combined with a lua backend for more complex logic."
    <> fullDesc

parseFlags :: IO Flags
parseFlags = execParser infoFlags
