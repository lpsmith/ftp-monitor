{-# LANGUAGE LambdaCase, OverloadedStrings, QuasiQuotes, RecordWildCards #-}

------------------------------------------------------------------------------
-- |
-- Module:      Cas
-- Copyright:   (c) 2016 Leon P Smith
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
-- A module for interacting with the Content-Addressable Store
--
------------------------------------------------------------------------------


module Cas (CasHandle(dataPath), open, close, withCas, withConn, withContent) where

-- import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad((>=>), join)
import           Data.ByteString ( ByteString )
-- import qualified Data.ByteString as B
import           Data.Function (on)
import           Data.List (groupBy)
import           System.IO (Handle)
import qualified System.IO as IO
import           System.IO.Error (isDoesNotExistError)
import           System.IO.Streams ( InputStream, takeBytes )
import qualified System.IO.Streams as Streams
import           System.FilePath ((</>))



import           Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as DB
import           SqlQQ

import           Hash (Hash)
-- import           Segment (Segment)
import qualified Segment as Seg
import           Util (dropBytes)


data CasHandle = CasHandle {
      dataPath :: Maybe FilePath,
      connRef  :: !(MVar DB.Connection)
    }

open :: Maybe FilePath -> IO CasHandle
open dataPath = do
  connRef <- newMVar =<< DB.open (maybe id (</>) dataPath "metadata.sqlite")
  return $ CasHandle{..}

close :: CasHandle -> IO ()
close cas_h = withConn cas_h DB.close

withCas :: Maybe FilePath -> (CasHandle -> IO a) -> IO a
withCas path = bracket (open path) close


openFileIfExists :: FilePath -> IO (Maybe Handle)
openFileIfExists path = handle dne (Just <$> IO.openFile path IO.ReadMode)
  where
    dne err = if isDoesNotExistError err
              then return Nothing
              else throw err

withFileIfExists :: FilePath -> (Maybe Handle -> IO a) -> IO a
withFileIfExists path = bracket (openFileIfExists path)
                                (maybe (return ()) IO.hClose)

withConn :: CasHandle -> (DB.Connection -> IO a) -> IO a
withConn ch = withMVar (connRef ch)

withContent :: CasHandle -> Hash -> (InputStream ByteString -> IO a) -> IO a
withContent ch = findFile
  where
    filePath hash = maybe id (</>) (dataPath ch) ("cas" </> show hash)

    findFile hash handler = join $ do
      withFileIfExists (filePath hash ++ ".gz") $ \case
        Nothing -> return (findSegment hash handler)
        Just h  -> do
          inS <- Streams.handleToInputStream h >>= Streams.gunzip
          a <- handler inS
          return (return a)

    findSegment hash handler = do
      segss_ <- withConn ch $ \conn -> do
         DB.query conn [sql|
           SELECT * FROM segments
            WHERE hash = ?
            ORDER BY sequence_number DESC, segment_number ASC
          |] (Only hash)
      case groupBy ((==) `on` Seg.sequence_number) segss_ of
        [] -> fail ("Cas.withContent: content not found for hash " ++ show hash)
        (segs:_) -> loop segs []
     where
      loop (seg:segs) inSs
        | Seg.length seg <= 0 = loop segs inSs
        | otherwise = do
          findFile (Seg.segment_hash seg) $
              dropBytes (Seg.offset seg) >=>
              takeBytes (Seg.length seg) >=> \inS -> do
                  loop segs (inS:inSs)
      loop [] inSs =
          Streams.concatInputStreams (reverse inSs) >>= handler
