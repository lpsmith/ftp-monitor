{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- |
-- Module:      Main
-- Copyright:   (c) 2016 Leon P Smith
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
------------------------------------------------------------------------------

module Main where

import Control.Applicative
import Control.Monad(void)
import Control.Exception(throwIO)
import TmpFile
import Data.List (isSuffixOf)
import System.Process
-- import System.Directory
import System.FilePath
import System.Environment
-- import System.IO (Handle)
-- import qualified System.IO as IO
-- import System.Posix.IO(fdToHandle, handleToFd, closeFd)
import System.Posix.Types(Fd(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import System.Exit
import qualified Data.Char as Char
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple.Types (Only(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL
import SqlQQ
import LsParser
import Data.Int(Int64)
import Data.Time
import Timestamp

showFd :: Fd -> String
showFd (Fd fd) = show fd

downloadFile :: String -> String -> String -> String -> IO ByteString
downloadFile user pass uri dest
  | ".gz" `isSuffixOf` uri = do
--     cwd  <- getCurrentDirectory
     tmpfd <- openTmpFileFd dest
     (Just sha_in, Just sha_out, _sha_err, sha_h) <-
         createProcess (proc "sha256sum" []) {
                             env     = Just [],
                             std_in  = CreatePipe,
                             std_err = Inherit,
                             std_out = CreatePipe
                          }
     (Just gz_in, Nothing, _gz_err, gz_h) <-
         createProcess (proc "gunzip" []) {
                             env     = Just [],
                             std_in  = CreatePipe,
                             std_err = Inherit,
                             std_out = UseHandle sha_in
                          }
     (Just tee_in, Nothing, _tee_err, tee_h) <-
         createProcess (proc "tee" ["/dev/fd/" ++ showFd tmpfd]) {
                             env     = Just [],
                             std_in  = CreatePipe,
                             std_err = Inherit,
                             std_out = UseHandle gz_in
                          }
     (Just _curl_in, Nothing {- Just curl_out -} , _curl_err, curl_h) <-
         createProcess (proc "curl" [ "--user", user ++ ":" ++ pass
                                    , "--ftp-ssl"
                                    , uri                           ]) {
                             env     = Just [],
                             std_in  = CreatePipe,
                             std_err = Inherit,
                             std_out = UseHandle tee_in

                          }
--     tee curl_out tmph gz_in
--     IO.hClose curl_out
--     tmpfd <- handleToFd tmph
--     IO.hClose gz_in
     curlExitCode <- waitForProcess curl_h
     teeExitCode  <- waitForProcess tee_h
     gzExitCode   <- waitForProcess gz_h
     shaExitCode  <- waitForProcess sha_h
     if    curlExitCode /= ExitSuccess || teeExitCode /= ExitSuccess
        || gzExitCode /= ExitSuccess   || shaExitCode /= ExitSuccess
     then fail "a subprocess failed"
     else do
       hex_string <- B8.take 64 <$> B8.hGetLine sha_out
       if    B8.length hex_string == 64
          && B8.all Char.isHexDigit hex_string
       then do
         linkTmpFileFd tmpfd (dest </> (B8.unpack hex_string ++ ".gz"))
         return hex_string
       else fail "failed to calculate sha256:  output not understood"


{--
tee :: Handle -> Handle -> Handle -> IO ()
tee inh outh1 outh2 = loop
  where loop = do
          bs <- B8.hGetSome inh 4096
          if B8.null bs
            then return ()
            else do
              B8.hPut outh1 bs
              B8.hPut outh2 bs
--}





createSchema :: DB.Connection -> IO ()
createSchema conn = do
    DB.execute_ conn [sql|
        CREATE TABLE IF NOT EXISTS file_metadata (
            id                INTEGER PRIMARY KEY,
            filename          TEXT,
            filehash          TEXT,
            hostname          TEXT,
            port              INT,
            username          TEXT,
            rdns_hostname     TEXT,
            ftp_size          INT,
            ftp_mtime         TEXT,
            fetch_start_time  INT,
            fetch_finish_time INT
          )
      |]
    DB.execute_ conn [sql|
        CREATE TABLE IF NOT EXISTS directory_listing_raw (
            id                INTEGER PRIMARY KEY,
            path              TEXT,
            listing_raw       TEXT,
            hostname          TEXT,
            port              INT,
            username          TEXT,
            rdns_hostname     TEXT,
            fetch_start_time  INT,
            fetch_finish_time INT
           )
      |]
    return ()

getFileData :: Connection -> T.Text -> IO (Maybe (LsTime, Int64))
getFileData conn filepath = do
    xs <- DB.query conn [sql|
            SELECT ftp_mtime, ftp_size
              FROM file_metadata
             WHERE filename = ?
             ORDER BY fetch_finish_time DESC LIMIT 1
          |] (Only filepath)
    case xs of
      [] -> return Nothing
      (x:_) -> return (Just x)

downloadListing :: String -> String -> String -> IO TL.Text
downloadListing user pass uri = do
     (Just _curl_in, Just curl_out, _curl_err, curl_h) <-
         createProcess (proc "curl" [ "--user", user ++ ":" ++ pass
                                    , "--ftp-ssl"
                                    , uri                           ]) {
                             env     = Just [],
                             std_in  = CreatePipe,
                             std_err = Inherit,
                             std_out = CreatePipe
                          }
     TLE.decodeUtf8' <$> BL.hGetContents curl_out >>= \case
        Left err -> do
          terminateProcess curl_h
          void $ waitForProcess curl_h
          throwIO err
        Right lst -> do
          curlExitCode <- waitForProcess curl_h
          if curlExitCode == ExitSuccess
          then return lst
          else fail "curl failed"



main :: IO ()
main = do
   -- TODO: replace this with something else?  
   -- (optparse-applicative?  config files?)
   username      <- getEnv "FTPMON_USERNAME"
   password      <- getEnv "FTPMON_PASSWORD"
   hostname      <- getEnv "FTPMON_HOSTNAME"
   port :: Int   <- maybe 21 read     <$> lookupEnv "FTPMON_PORT"
   rdns_hostname <- maybe hostname id <$> lookupEnv "FTPMON_RDNS_HOSTNAME" 
   path          <- maybe "/" id      <$> lookupEnv "FTPMON_FTP_PATH"
   data_path     <- getEnv "FTPMON_DATA_PATH"   
   let base_uri = "ftp://" ++ rdns_hostname ++ ":" ++ show port ++ path
   conn <- DB.open (data_path </> "metadata.sqlite")
   createSchema conn
   fetch_start_time <- getCurrentTime
   listing_raw <- downloadListing username password base_uri
   fetch_end_time <- getCurrentTime
   void $ DB.execute conn [sql|
        INSERT INTO directory_listing_raw
                  ( path
                  , listing_raw
                  , hostname
                  , port
                  , username
                  , rdns_hostname
                  , fetch_start_time
                  , fetch_finish_time
                  ) VALUES (?,?,?,?,?,?,?,?)
      |]          ( path
                  , listing_raw
                  , hostname
                  , port
                  , username
                  , rdns_hostname
                  , utcToTSTZ fetch_start_time
                  , utcToTSTZ fetch_end_time
                  )

   let handleFile :: Entry -> IO ()
       handleFile Entry{..}
         | entry_type == File && T.isSuffixOf ".gz" entry_name = do
           getFileData conn entry_name >>= \case
             Just (mtime, size)
                 |    (entry_mtime `equiv` mtime)
                   && (entry_size == size)
                 -> return ()
             _   -> do
               fetch_start_time <- getCurrentTime
               filehash <-
                   downloadFile username password
                                (base_uri ++ T.unpack entry_name)
                                (data_path </> "cas")
               fetch_finish_time <- getCurrentTime
               DB.execute conn [sql|
                   INSERT INTO file_metadata
                             ( filename
                             , filehash
                             , hostname
                             , port
                             , username
                             , rdns_hostname
                             , ftp_size
                             , ftp_mtime
                             , fetch_start_time
                             , fetch_finish_time
                             ) VALUES (?,?,?,?,?,?,?,?,?,?)
                 |] ( entry_name
                    , filehash
                    , hostname
                    , port
                    , username
                    , rdns_hostname
                    , entry_size
                    , entry_mtime
                    , utcToTSTZ fetch_start_time
                    , utcToTSTZ fetch_finish_time
                    )
               return ()
         | otherwise = return ()

   case parseListing (utctDay fetch_end_time) listing_raw of
     Left err -> print err
     Right entries -> mapM_ handleFile entries
