{-# LANGUAGE ForeignFunctionInterface #-}
module System.Chroot
    ( fchroot
    , useEnv
    , forceList
    , forceList'
    ) where

import Control.Exception

import qualified Data.ByteString.Char8 as B

import Foreign.C.Error
import Foreign.C.String

import System.Cmd (system)
import System.Directory (createDirectoryIfMissing)
import System.Exit
import System.FilePath (dropTrailingPathSeparator, dropFileName)
import System.IO (hPutStr, stderr)
import System.Posix.Env (getEnv)
import System.Posix.IO
import System.Posix.Directory

import System.Unix.Process (Output(..))

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
useEnv :: FilePath -> (a -> IO a) -> IO a -> IO a
useEnv rootPath force action =
    do sockPath <- try (getEnv "SSH_AUTH_SOCK") >>= either (error . show) return
       home <- try (getEnv "HOME") >>= either (error . show) return
       copySSH home
       -- We need to force the output before we exit the changeroot.
       -- Otherwise we lose our ability to communicate with the ssh
       -- agent and we get errors.
       withSock sockPath . fchroot rootPath $ (action >>= force)
    where
      copySSH Nothing = return ()
      copySSH (Just home) =
          -- Do NOT preserve ownership, files must be owned by root.
          system' ("rsync -rlptgDHxS --delete " ++ home ++ "/.ssh/ " ++ rootPath ++ "/root/.ssh")
      withSock Nothing action = action
      withSock (Just sockPath) action =
          withMountBind dir (rootPath ++ dir) action
          where dir = dropTrailingPathSeparator (dropFileName sockPath)
      withMountBind toMount mountPoint action =
          try doMount >>= either (error . show) return
          where
            doMount =
                do createDirectoryIfMissing True mountPoint
                   system' $ "mount --bind " ++ escapePathForMount toMount ++ " " ++ escapePathForMount mountPoint
                   result <- action
                   system' $ "umount " ++ escapePathForMount mountPoint
                   return result
      escapePathForMount = id	-- FIXME - Path arguments should be escaped
      system' s =
          system s >>= testcode
          where testcode (ExitFailure n) = error (show s ++ " -> " ++ show n)
                testcode ExitSuccess = return ()

forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output

forceList' :: [Output] -> IO [Output]
forceList' output = printOutput output >>= forceList

-- |Print all the output to the appropriate output channel
printOutput :: [Output] -> IO [Output]
printOutput output =
    mapM print output
    where
      print x@(Stdout s) = putStr (B.unpack s) >> return x
      print x@(Stderr s) = hPutStr stderr (B.unpack s) >> return x
      print x = return x

