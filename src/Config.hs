{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Config
    ( FetchSource(..)
    , parseFetchSources
    ) where

import Control.Applicative
import Control.Monad(forM)
import Data.Text (Text)
-- import qualified Data.Text as T
import Data.Configurator ()
import Data.Configurator.Parser

data FetchSource = FetchSource {
      username      :: String
   ,  password      :: String
   ,  hostname      :: String
   ,  port          :: Int
   ,  rdns_hostname :: String
   ,  ftp_directory :: String
   }

instance Monad ConfigParserA where
   return = pure
   (>>=) = unsafeBind

parseFetchSource :: ConfigParser m => m FetchSource
parseFetchSource =
    parserA $ do
      username      <- required "username"
      password      <- required "password"
      hostname      <- required "hostname"
      port          <- required "port"
      rdns_hostname <- required "rdns_hostname"
      ftp_directory <- required "ftp_directory"
      pure FetchSource{..}

parseFetchSources :: ConfigParser m => m [(Text, Maybe FetchSource)]
parseFetchSources =
    parserM $ do
        sources <- subgroups ""
        forM sources $ \name ->
            localConfig (subconfig name) $ do
                config <- recover parseFetchSource
                return (name, config)
