{-# LANGUAGE ForeignFunctionInterface #-}
module System.Chroot
    ( fchroot
    , useEnv
    ) where

import Control.Exception

import Foreign.C.Error
import Foreign.C.String

import System.Cmd (system)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropTrailingPathSeparator, dropFileName)
import System.Posix.Env (getEnv)
import System.Posix.IO
import System.Posix.Directory

foreign import ccall unsafe "chroot" c_chroot :: CString -> IO Int

-- |chroot changes the root directory to filepath
-- NOTE: it does not change the working directory, just the root directory
-- NOTE: will throw IOError if chroot fails
chroot :: FilePath -> IO ()
chroot fp = withCString fp $ \cfp -> throwErrnoIfMinus1_ "chroot" (c_chroot cfp)

-- |fchroot runs an IO action inside a chroot
-- fchroot performs a chroot, runs the action, and then restores the
-- original root and working directory. This probably affects the
-- chroot and working directory of all the threads in the process,
-- so...
-- NOTE: will throw IOError if internal chroot fails
fchroot :: FilePath -> IO a -> IO a
fchroot path action =
    do origWd <- getWorkingDirectory
       rootFd <- openFd "/" ReadOnly Nothing defaultFileFlags
       chroot path
       changeWorkingDirectory "/"
       action `finally` (breakFree origWd rootFd)
    where
      breakFree origWd rootFd =
          do changeWorkingDirectoryFd rootFd
             closeFd rootFd
             chroot "."
             changeWorkingDirectory origWd

-- |The ssh inside of the chroot needs to be able to talk to the
-- running ssh-agent.  Therefore we mount --bind the ssh agent socket
-- dir inside the chroot (and umount it when we exit the chroot.
useEnv :: FilePath -> IO a -> IO a
useEnv path action =
    getEnv "SSH_AUTH_SOCK" >>= useSock
    where
      useSock Nothing =
          fchroot path action
      useSock (Just sock) = 
          do let dir = dropTrailingPathSeparator (dropFileName sock)
             createDirectoryIfMissing True (path ++ dir)
             system ("mount --bind " ++ dir ++ " " ++ path ++ dir)
             result <- fchroot path action
             system ("umount " ++ path ++ dir)
             return result
