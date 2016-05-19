{-# LANGUAGE BangPatterns, RecordWildCards #-}

------------------------------------------------------------------------------
-- |
-- Module:      LsParser
-- Copyright:   (c) 2016 Leon P Smith
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
------------------------------------------------------------------------------

module LsParser where

import Control.Applicative
import Data.Time
import Control.Monad (void)
-- import System.Locale
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple(SQLData(..))
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Attoparsec.Text(Parser)
import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Text.Lazy as LP
import qualified Data.Char as Char
import Data.Int(Int64)

data EntryType = Directory
               | File
               | Unknown !Char
                 deriving (Eq, Show)

data LsTime = LsTime Day (Maybe TimeOfDay) deriving (Show)

data Entry = Entry {
      entry_type  :: !EntryType
    , entry_size  :: !Int64
    , entry_mtime :: !LsTime
    , entry_name  :: !Text
    } deriving (Show)

parseListing :: Day -> TL.Text -> Either String [Entry]
parseListing ctx = LP.eitherResult . LP.parse (pListing ctx)

pListing :: Day -> Parser [Entry]
pListing ctx_day = P.many' (pListingLine ctx_day)

pListingLine :: Day -> Parser Entry
pListingLine ctx_day = do
  c <- P.anyChar
  let entry_type = case c of
                     'd' -> Directory
                     '-' -> File
                     _   -> Unknown c
  _permissions <- P.take 9
  void P.space *> P.skipSpace
  _links <- P.takeWhile1 Char.isDigit
  void P.space *> P.skipSpace
  _user <- P.takeWhile1 Char.isAlpha
  void P.space *> P.skipSpace
  _group <- P.takeWhile1 Char.isAlpha
  void P.space *> P.skipSpace
  !entry_size <- read . T.unpack <$> P.takeWhile1 Char.isDigit
  void P.space *> P.skipSpace
  !entry_mtime <- pLsTime ctx_day
  void P.space
  entry_name <- P.takeWhile1 ('\n' /=)
  void $ P.anyChar
  return $! Entry{..}

equiv :: LsTime -> LsTime -> Bool
equiv (LsTime d0 t0) (LsTime d1 t1) =
    (d0 == d1) && (t0 == Nothing || t1 == Nothing || t0 == t1)

instance FromField LsTime where
    fromField fld =
        case fieldData fld of
          (SQLText txt) ->
              case readSTime True defaultTimeLocale "%F" (T.unpack txt) of
                [(d, str)] ->
                    case readSTime True defaultTimeLocale "%R" str of
                      [(t, str0)] | [("","")] <- lex str0 ->
                          pure (LsTime d (Just t))
                      _ | [("","")] <- lex str ->
                          pure (LsTime d Nothing)
                        | otherwise ->
                          returnError ConversionFailed fld "time of day"
                _ -> returnError ConversionFailed fld "date"
          SQLNull -> returnError UnexpectedNull fld ""
          _       -> returnError Incompatible   fld ""

instance ToField LsTime where
    toField (LsTime d mt) =
        case mt of
          Nothing -> SQLText (T.pack (fmt "%F" d))
          Just t  -> SQLText (T.pack (fmt "%F" d ++ " " ++ fmt "%R" t))
      where
         fmt x = formatTime defaultTimeLocale x

-- | The parameter is the day the listing was generated,  in order
--   to disambiguate dates that don't include the year.   Only
--   needs to be accurate to within a month.

pLsTime :: Day -> Parser LsTime
pLsTime ctx_day = do
    m <- (monthFromAbbr . T.unpack <$> P.take 3) >>=
            maybe (fail "invalid month abbreviation") pure
    d  <- P.space *> P.skipSpace *> oneOrTwoDigits
    ty <- P.space *> P.skipSpace *> timeOfDayOrYear
    case ty of
      Left t  -> let (ctx_y, ctx_m, _ctx_d) = toGregorian ctx_day
                     y | m <= ctx_m + 1 = ctx_y
                       | otherwise      = ctx_y - 1
                  in case fromGregorianValid y m d of
                       Nothing  -> fail "invalid gregorian date"
                       Just day -> pure $! LsTime day (Just t)
      Right y -> case fromGregorianValid (fromIntegral y) m d of
                   Nothing  -> fail "invalid gregorian date"
                   Just day -> pure $! LsTime day Nothing
  where
    fromDigit c = Char.ord c - Char.ord '0'
    oneOrTwoDigits = do
        x <- fromDigit <$> P.digit
        let mOneDigit = do
               y <- fromDigit <$> P.digit
               pure $! (10*x + y)
        mOneDigit <|> return x

    twoDigits = do
        x <- fromDigit <$> P.digit
        y <- fromDigit <$> P.digit
        pure $! (10*x + y)

    timeOfDayOrYear = do
      h <- twoDigits
      c <- P.peekChar
      case c of
        Just ':' -> do
           m <- P.anyChar *> twoDigits
           case makeTimeOfDayValid h m 0 of
             Nothing  -> fail "invalid time of day"
             Just tod -> pure $! Left tod
        _   -> do
           m <- twoDigits
           pure $! Right (100 * h + m)

monthFromAbbr :: String -> Maybe Int
monthFromAbbr x =
  case map Char.toUpper x of
    "JAN" -> Just  1
    "FEB" -> Just  2
    "MAR" -> Just  3
    "APR" -> Just  4
    "MAY" -> Just  5
    "JUN" -> Just  6
    "JUL" -> Just  7
    "AUG" -> Just  8
    "SEP" -> Just  9
    "OCT" -> Just 10
    "NOV" -> Just 11
    "DEC" -> Just 12
    _     -> Nothing
