------------------------------------------------------------------------------
-- |
-- Module:      CommandLine
-- Copyright:   (c) 2016 Leon P Smith
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
------------------------------------------------------------------------------

module CommandLine where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T

import Options.Applicative
import Options.Applicative.Extra(execParser)
import Hash
import System.Directory

data CommandLine = CommandLine {
      dataPath    :: (Maybe FilePath),
      commandSpec :: CommandSpec
    }

data CommandSpec =
     Fetch FetchOptions
   | Ls    LsOptions
   | Cat   CatOptions


data FetchOptions = FetchOptions {
      fetch_source :: Text
    }

fetchOptions :: Parser FetchOptions
fetchOptions = FetchOptions . T.pack <$> argument str (metavar "SOURCE")

type LsOptions = ()

lsOptions :: Parser LsOptions
lsOptions = pure ()

data CatOptions = CatOptions {
      cat_target :: !(Either Text Hash)
    }

catOptions :: Parser CatOptions
catOptions = CatOptions <$> ((Left <$> filearg) <|> (Right <$> hasharg))
  where
    filearg = T.pack <$> argument str (metavar "FILE")
    hasharg = option auto (long "hash" <> metavar "HASH")

parseCommandSpec :: Parser CommandSpec
parseCommandSpec =
  subparser
    (  command "fetch" (info (Fetch <$> fetchOptions)
         ( progDesc "fetch any new or obviously changed files" ))
    <> command "ls" (info (Ls <$> lsOptions)
         ( progDesc "list the files that are available" ))
    <> command "cat" (info (Cat <$> catOptions)
         ( progDesc "print the content of a file or hash to standard output" ))
    )

parseCommandLine :: Parser CommandLine
parseCommandLine =
    CommandLine <$> optional (strOption (long "data-dir" <> metavar "DATA_DIR"))
              <*> parseCommandSpec

commandLine :: ParserInfo CommandLine
commandLine =
  info (helper <*> parseCommandLine)
   (  fullDesc
   <> progDesc "particularly geared to log files"
   <> header   "ftp-monitor - a ftp directory change tracker"
   )

execCommandLine :: IO CommandLine
execCommandLine = execParser commandLine
