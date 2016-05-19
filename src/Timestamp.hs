{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Timestamp where

import Data.Int
import Data.Time
import Database.SQLite.Simple(SQLData(..))
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

newtype TSTZ = TSTZ {unTSTZ :: Int64} deriving (Eq,Ord)

epoch :: UTCTime
epoch = read "2000-01-01 00:00:00"

utcToTSTZ :: UTCTime -> TSTZ
utcToTSTZ t = TSTZ (round ((t `diffUTCTime` epoch) * 1000000))

tstzToUTC :: TSTZ -> UTCTime
tstzToUTC (TSTZ t) = (fromIntegral t / 1000000) `addUTCTime` epoch

instance FromField TSTZ where
    fromField fld =
        case fieldData fld of
          (SQLInteger n) -> pure (TSTZ n)
          SQLNull -> returnError UnexpectedNull fld ""
          _       -> returnError Incompatible   fld ""

instance ToField TSTZ where
    toField (TSTZ n) = SQLInteger n
