{-# LANGUAGE LambdaCase #-}

------------------------------------------------------------------------------
-- |
-- Module:      Util
-- Copyright:   (c) 2016 Leon P Smith
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
------------------------------------------------------------------------------

module Util where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Int
import           Data.IORef
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams

dropBytes :: Int64 -> InputStream ByteString -> IO (InputStream ByteString)
dropBytes len_ inS
  | len_ <= 0 = return inS
  | otherwise = do
    lenRef <- newIORef len_
    Streams.makeInputStream $ do
      len <- readIORef lenRef
      if len <= 0
      then Streams.read inS
      else let loop n = do
                   Streams.read inS >>= \case
                     Nothing -> return Nothing
                     Just str -> if n < fromIntegral (B.length str)
                                 then do
                                   return $! Just $! B.drop (fromIntegral n) str
                                 else do
                                   loop (n - fromIntegral (B.length str))
            in loop len
