{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Temp
    ( Download(..)
    , Target(..)
    , asTarget
    ) where

import Control.Exception (SomeException, try)
import Data.Time (NominalDiffTime)
import Data.Version (Version)
import qualified Debian.AutoBuilder.BuildTarget.Common as C
import Debian.AutoBuilder.Types.RetrieveMethod (RetrieveMethod(..))
import Debian.Repo.SourceTree (DebianBuildTree(debTree'), DebianSourceTree, findOneDebianBuildTree, findDebianSourceTree)
import System.Unix.Process
import System.IO (hPutStrLn, stderr)

data Download
    = Download
      { method' :: RetrieveMethod
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
      }

instance C.Download Download where
    method = method'
    getTop _ tgt = getTop tgt
    revision _ tgt = return (revision tgt)
    logText x _ = logText x

-- | A replacement for the BuildTarget class and the BuildTarget.* types.  The method code
-- moves into the function that turns a RetrieveMethod into a BuildTarget.
data Target
    = Target
      { download :: Download
      , debianSourceTree :: DebianSourceTree
      -- ^ Return the debian source tree.  Every target must have
      -- this, since this program only builds debian packages.
      }

instance C.Download Target where
    method = C.method . download
    getTop x tgt = C.getTop x (download tgt)
    revision x tgt = C.revision x (download tgt)
    logText tgt x = C.logText (download tgt) x
    mVersion tgt = C.mVersion (download tgt)
    origTarball x tgt = C.origTarball x (download tgt)

instance C.BuildTarget Target where
    debianSourceTree tgt = debianSourceTree tgt

-- | Try to turn a Download into a Target.  This will throw an
-- exception if there is not a valid debian source tree at the
-- location in getTop.
asTarget :: Download -> IO Target
asTarget download =
    try (findOneDebianBuildTree (C.getTop (error "asTarget: unused getTop argument") download) >>=
         maybe (findDebianSourceTree (C.getTop (error "asTarget: unused getTop argument") download)) (return . debTree')) >>=
    either (\ (e :: SomeException) -> let msg = "asTarget " ++ show (C.method download) ++ " :" ++ show e in hPutStrLn stderr msg >> error msg)
           (\ tree -> return $ Target { download = download
                                      , debianSourceTree = tree })
       -- tarball <- findOrigTarball tree
       