{-# LANGUAGE LambdaCase #-}

module Args
  ( -- * Configuration Types
    Args (..)
  , FileArg (..)
    -- * I/O Interface
  , parse
    -- * Command-Line Parser
  , parser
  ) where

import Options.Applicative


-- | A file argument.
data FileArg
  = StdIn
    -- ^ Read from standard input.
  | File FilePath
    -- ^ Read from the given file.
  deriving (Eq, Show)


-- | Parsed command-line arguments.
data Args = Args
  { number :: Bool
    -- ^ Number output lines flag.
  , files :: [FileArg]
    -- ^ The list of input files.
  } deriving (Eq, Show)


-- | Parse the command-line arguments.
parse :: IO Args
parse = execParser parser


-- | Command-line parser.
parser :: ParserInfo Args
parser =
  info (argsParser <**> helper)
    ( fullDesc
    <> progDesc "Concatenate files to standard output."
    <> header "cat_hs - A Haskell implementation of cat." )


argsParser :: Parser Args
argsParser = Args
  <$> switch
    ( long "number"
    <> short 'n'
    <> help "Number all output lines." )
  <*> many fileArgParser


fileArgParser :: Parser FileArg
fileArgParser =
  argument readFileArg
    ( metavar "FILES..."
    <> help
      "Read from the given file, or from standard input if '-'.\
      \ Use './-' to read from a file named '-'." )


-- | Read a 'FileArg' from a 'String'.
readFileArg :: ReadM FileArg
readFileArg =
  maybeReader $ \case
    "-" -> Just StdIn
    fname -> Just $ File fname
