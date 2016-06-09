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
import Control.Monad(void, forM_)
import Control.Exception(throwIO)
import TmpFile
import Data.List (isSuffixOf)
import System.Process
-- import System.Directory
import System.FilePath
import System.Environment
-- import System.IO (Handle)
import qualified System.IO as IO
import           System.IO.Error (isAlreadyExistsError)
-- import System.Posix.IO(fdToHandle, handleToFd, closeFd)
import System.Posix.Types(Fd(..))
-- import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
-- import qualified Data.ByteString.Base16 as Base16
import System.Exit
-- import qualified Data.Char as Char
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple.Types (Only(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Configurator as Config
import qualified Data.Configurator.Parser as Config
import SqlQQ
import LsParser
import Data.Int(Int64)
import Data.Time
import Timestamp
import Hash(Hash)
import qualified Hash
import CommandLine ( CommandLine(..)
                   , CommandSpec(..)
                   , FetchOptions(..)
                   , LsOptions(..)
                   , CatOptions(..)
                   , execCommandLine
                   )
import Cas (CasHandle, withConn)
import qualified Cas
import qualified Segment
import qualified System.IO.Streams as Streams
import Config (FetchSource(..), parseFetchSources)
import Util (ignoreError)

showFd :: Fd -> String
showFd (Fd fd) = show fd

downloadFile :: String -> String -> String -> String -> IO Hash
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
       hex_string <- B8.hGetContents sha_out
       case Hash.mkSHA256Hash hex_string of
         Just hash -> do
             ignoreError isAlreadyExistsError $ do
                 linkTmpFileFd tmpfd (dest </> (show hash ++ ".gz"))
             return hash
         Nothing -> do
             fail "failed to calculate sha256:  output not understood"

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
            filehash          BLOB,
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
    DB.execute_ conn Segment.createTable
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

doFetch :: CasHandle -> FetchOptions -> IO ()
doFetch cas_h opts = do
   let path = maybe id (</>) (Cas.dataPath cas_h) "sources.config"
   cc <- Config.load [Config.Required path]
   conf <- Config.readConfig cc
   case Config.runParserM parseFetchSources conf of
     (Nothing, errs) -> do
         IO.hPutStr IO.stderr "Parse of sources.config failed:\n\n"
         forM_ errs (IO.hPrint IO.stderr)
         exitFailure
     (Just sources, errs) -> do
         let source_name = fetch_source opts
         case lookup source_name sources of
           Nothing -> do
             IO.hPutStr IO.stderr
                   ("Source " ++ T.unpack source_name ++ " not found\n")
             exitFailure
           Just Nothing -> do
             IO.hPutStr IO.stderr
                   ("Source " ++ T.unpack source_name ++ " parse error\n")
             -- It might be nice to filter errs to be relevant only to the
             -- subconfig in question.  On the one hand, configurator-ng
             -- doesn't exactly support this cleanly.   On the other,
             -- it doesn't seem that important (yet?) in this use case.
             forM_ errs (IO.hPrint IO.stderr)
             exitFailure
           Just (Just source) -> do
             doFetch2 cas_h opts source

doFetch2 :: CasHandle -> FetchOptions -> FetchSource -> IO ()
doFetch2 cas_h _opts FetchSource{..} = do
   let base_uri = "ftp://" ++ rdns_hostname ++ ":" ++ show port ++ ftp_directory
   withConn cas_h createSchema
   fetch_start_time <- getCurrentTime
   listing_raw <- downloadListing username password base_uri
   fetch_end_time <- getCurrentTime
   withConn cas_h $ \conn -> do
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
       |]         ( ftp_directory
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
           withConn cas_h (\conn -> getFileData conn entry_name) >>= \case
             Just (mtime, size)
                 |    (entry_mtime `equiv` mtime)
                   && (entry_size == size)
                 -> return ()
             _   -> do
               fetch_start_time <- getCurrentTime
               filehash <-
                   downloadFile username password
                                (base_uri ++ T.unpack entry_name)
                                (maybe id (</>) (Cas.dataPath cas_h) "cas")
               fetch_finish_time <- getCurrentTime
               withConn cas_h $ \conn -> DB.execute conn [sql|
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
     Left err -> do
         IO.hPrint IO.stderr err
         exitFailure
     Right entries -> mapM_ handleFile entries

doLs :: CasHandle -> LsOptions -> IO ()
doLs cas_h _opts =
    withConn cas_h $ \conn -> do
      files <- DB.query_ conn [sql|
                   SELECT DISTINCT filename FROM file_metadata
                    ORDER BY filename
                |]
      forM_ files $ \(Only file) -> B8.putStrLn (TE.encodeUtf8 file)

doCat :: CasHandle -> CatOptions -> IO ()
doCat cas_h opts =
    case cat_target opts of
      Left filename -> catFilename filename
      Right hash    -> catHash mkHashErrMsg hash
  where
    catFilename filename = do
        withConn cas_h $ \conn -> do
          hash_ <- DB.query conn [sql|
             SELECT filehash FROM file_metadata
              WHERE filename = ?
              ORDER BY fetch_finish_time DESC
              LIMIT 1
            |] (Only filename)
          case hash_ of
            [] -> do
              IO.hPutStrLn IO.stderr
                   ("ftp-monitor: cannot access " ++ T.unpack filename
                    ++ ": No such file")
              exitFailure
            (Only hash : _) ->
              catHash (mkFileErrMsg filename) hash

    catHash mkErrMsg hash = do
        Cas.withContent cas_h hash $ \inS_ -> do
            (inS, getHash') <- Hash.sha256stream inS_
            Streams.connect inS Streams.stdout
            hash' <- getHash'
            if hash' == hash
            then exitSuccess
            else do
              IO.hPutStrLn IO.stderr ("ftp-monitor: integrity check failure" ++ mkErrMsg hash hash')
              exitFailure

    mkFileErrMsg filename hash hash' =
        "\n    file  " ++ T.unpack filename ++ mkHashErrMsg hash hash'

    mkHashErrMsg hash hash' =
        "\n    hash  " ++ show hash  ++
        "\n    hash' " ++ show hash' ++
        "\n"


main :: IO ()
main = do
   cmdLine <- execCommandLine
   data_path <- liftA2 (<|>) (pure (dataPath cmdLine)) (lookupEnv "FTPMON_DATA_PATH")
   Cas.withCas data_path $ \cas_h ->
     case commandSpec cmdLine of
       Ls    opts -> doLs    cas_h opts
       Cat   opts -> doCat   cas_h opts
       Fetch opts -> doFetch cas_h opts
