{-# LANGUAGE BangPatterns, OverloadedStrings, QuasiQuotes, RecordWildCards #-}

------------------------------------------------------------------------------
-- |
-- Module:      Segment
-- Copyright:   (c) 2016 Leon P Smith
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
------------------------------------------------------------------------------

module Segment where

import Data.Int(Int64)
import Hash

import Database.SQLite.Simple as DB
import Database.SQLite.Simple.FromRow
import SqlQQ

data Segment = Segment {
      hash            :: !Hash,
      sequence_number :: !Int64,
      segment_number  :: !Int64,
      segment_hash    :: !Hash,
      offset          :: !Int64,
      length          :: !Int64
    }

instance FromRow Segment where
   fromRow = Segment <$> field <*> field <*> field
                     <*> field <*> field <*> field

insert :: Connection -> Segment -> IO ()
insert conn Segment{..} = do
  DB.execute conn [sql|
        INSERT INTO segment VALUES (?,?,?,?,?,?)
     |] (hash, sequence_number, segment_hash, offset, length)

createTable :: Query
createTable = [sql|
        CREATE TABLE IF NOT EXISTS segment (
            hash              BLOB,
            sequence_number   INTEGER,
            segment_number    INTEGER,
            segment_hash      BLOB,
            offset            INTEGER,
            length            INTEGER,
            PRIMARY KEY(hash, sequence_number, segment_number)
         )
   |]
