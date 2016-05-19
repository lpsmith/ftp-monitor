{-# LANGUAGE ForeignFunctionInterface #-}

------------------------------------------------------------------------------
-- |
-- Module:      TmpFile
-- Copyright:   (c) 2016 Leon P Smith
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
-- Quick and dirty binding to Linux 3.11 and later's O_TMPFILE
--
------------------------------------------------------------------------------

module TmpFile where

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

import Data.Bits
import Foreign.C
import System.Posix.Types

-- | Create an inode without a corresponding directory entry on the filesystem
--   of the given directory:

openTmpFileFd :: FilePath -> IO Fd
openTmpFileFd path =
    withCString path $ \str ->
      throwErrnoIfMinus1Retry "TmpFile.openTmpFileFd" $
        c_open str
          ((#const __O_TMPFILE) .|. (#const O_DIRECTORY) .|. (#const O_RDWR))
          ((#const S_IRUSR) .|. (#const S_IWUSR))

-- | Link an inode created by 'openTmpFile' to a given filename:

linkTmpFileFd :: Fd -> FilePath -> IO ()
linkTmpFileFd (Fd fd) path =
    withCString ("/proc/self/fd/" ++ show fd) $ \source ->
      withCString path $ \dest ->
        throwErrnoIfMinus1_ "TmpFile.linkTmpFileFd" $
          c_linkat (#const AT_FDCWD) source (#const AT_FDCWD) dest (#const AT_SYMLINK_FOLLOW)

foreign import ccall safe "open"   c_open   :: CString -> CInt -> CInt -> IO Fd
foreign import ccall safe "linkat" c_linkat :: Fd -> CString
                                            -> Fd -> CString
                                            -> CInt -> IO CInt
