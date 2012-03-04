{-# LANGUAGE RankNTypes #-}
module Debian.AutoBuilder.Types.Download
    ( Download(..)
    ) where

import Data.Time (NominalDiffTime)
import Data.Version (Version)
import Debian.AutoBuilder.Types.RetrieveMethod (RetrieveMethod(..))
import System.Unix.Process

data Download
    = Download
      { method :: RetrieveMethod
      -- ^ The method used to retrieve this target.
      , getTop :: FilePath
      -- ^ The directory containing the target's files.  For most target types, these
      --  files could be anything, not necessarily a Debian source directory.
      , revision :: String
      -- ^ The revision is a string to be used as the /Revision:/
      -- attribute of the source package information.  This is
      -- intended to characterize the build environment of the
      -- package, including some string describing the package's
      -- current revision in the revision control system, and the
      -- versions of the build dependencies that were installed when
      -- the package was build.  If the package is not in a revision
      -- control system its upstream version number is used.
      , mVersion :: Maybe Version
      -- ^ Some targets can return a debian version, use this to retrieve it.
      , origTarball :: Maybe FilePath
      -- ^ If we have access to an original tarball, this returns its path.
      , logText :: String
      -- ^ Text to include in changelog entry.
      , cleanTarget :: FilePath -> IO ([Output], NominalDiffTime)
      -- ^ Clean version control info out of a target after it has
      -- been moved to the given location.
      , buildWrapper :: forall a. IO a -> IO a
      -- ^ Modify the build process in some way - currently only the
      -- proc target modifies this by mounting and then unmounting /proc.
      }