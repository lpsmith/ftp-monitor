{-# LANGUAGE BangPatterns, ViewPatterns, LambdaCase #-}

------------------------------------------------------------------------------
-- |
-- Module:      Hash
-- Copyright:   (c) 2016 Leon P Smith
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
-- Various utilities and integrations for handling SHA256 digests
--
------------------------------------------------------------------------------

module Hash (Hash, mkSHA256Hash, showBS, sha256stream) where

import Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Char as Char
import Data.IORef
import Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple(SQLData(..))
import qualified Data.ByteArray as ByteArray
import           Crypto.Hash
                   ( Context
                   , SHA256
                   , hashInit
                   , hashUpdate
                   , hashFinalize
                   )
import qualified System.IO.Streams as Streams
import           System.IO.Streams.Internal (InputStream(..))

newtype Hash = SHA256Hash ByteString deriving (Eq, Ord)

mkSHA256Hash :: ByteString -> Maybe Hash
mkSHA256Hash hex_hash
    | B.length hash == 32 = Just (SHA256Hash hash)
    | otherwise           = Nothing
  where
    (hash, _remainder) = Base16.decode hex_hash

showBS :: Hash -> ByteString
showBS (SHA256Hash hash) = Base16.encode hash

instance Show Hash where
    show = B8.unpack . showBS

instance Read Hash where
   readsPrec _ str =
       case mkSHA256Hash (B8.pack (take 64 str)) of
         Nothing -> []
         Just hash -> case drop 64 str of
                        (c:_cs) | Char.isHexDigit c -> []
                        rest -> [(hash,rest)]

instance FromField Hash where
    fromField fld =
        case fieldData fld of
          (SQLBlob hash) ->
              if B.length hash == 32
              then pure (SHA256Hash hash)
              else returnError ConversionFailed fld "length not 32"
          SQLNull -> returnError UnexpectedNull fld ""
          _       -> returnError Incompatible   fld ""

instance ToField Hash where
    toField (SHA256Hash hash) = (SQLBlob hash)


data Ctx = Ctx !(Context SHA256) !ByteString

-- | Compute the sha256 of an inputstream.  This inputstream supports limited
--   pushback:  Don't push back more bytes than returned by the previous read,
--   otherwise this will result in an exception.

sha256stream :: InputStream ByteString -> IO (InputStream ByteString, IO Hash)
sha256stream inS = do
    ctxRef <- newIORef $ Ctx hashInit B.empty
    pbRef  <- newIORef []
    let updContext (Ctx ctx str) = hashUpdate ctx str
        updCtx strs ctx =  Ctx (updContext ctx) strs
        readHash = do
             ctx <- readIORef ctxRef
             return $! SHA256Hash (ByteArray.convert (hashFinalize (updContext ctx)))

        read = do
          readIORef pbRef >>= \case
            (str:strs) -> do
                writeIORef pbRef strs
                modifyIORef' ctxRef (updCtx str)
                return (Just str)
            [] -> do
                Streams.read inS >>= \case
                  Nothing -> do
                      modifyIORef' ctxRef (updCtx B.empty)
                      return Nothing
                  Just str -> do
                      modifyIORef' ctxRef (updCtx str)
                      return (Just str)

        tooMuchMsg = "Hash.sha256stream: attempted to put back too many bytes into input stream"
        trimCtx (B.length -> n) (Ctx ctx str)
           = let len = B.length str
              in if n > len
                 then error tooMuchMsg
                 else Ctx ctx (B.take (len - n) str)

        unRead str =
           if B.null str
           then return ()
           else do
             modifyIORef' ctxRef (trimCtx str)
             modifyIORef' pbRef (str:)

        !inS' = InputStream read unRead
    return $! (inS', readHash)
