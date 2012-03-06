{-# LANGUAGE RankNTypes #-}
module Debian.AutoBuilder.Types.Download
    ( Download(..)
    ) where

import Data.Time (NominalDiffTime)
import Data.Version (Version)
import Debian.AutoBuilder.Types.Packages (PackageFlag, RetrieveMethod(..))
import System.Unix.Process

data Download
    = Download
      { method :: RetrieveMethod
      -- ^ The method used to retrieve this target.
      , flags :: [PackageFlag]
      -- ^ The flags assocated with the package
      , getTop :: FilePath
      -- ^ The directory containing the target's files.  For most target types, these
      --  files could be anything, not necessarily a Debian source directory.
      , mVersion :: Maybe Version
      -- ^ Some targets can return a cabal version, use this to retrieve it.
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
