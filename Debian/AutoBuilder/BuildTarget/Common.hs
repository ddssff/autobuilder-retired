-- | BuildTarget represents the type class of methods for obtaining a
-- SourceTree: tla, apt, darcs, etc.
{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, FlexibleContexts #-}
module Debian.AutoBuilder.BuildTarget.Common
    ( Download(..)
    , md5sum
    , mustParseURI
    ) where

import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Char (ord)
import Data.Time (NominalDiffTime)
import Data.Version (Version)
import Debian.AutoBuilder.Types.RetrieveMethod (RetrieveMethod)
import Happstack.Crypto.MD5 (md5)
import Network.URI (URI, parseURI)
import System.Unix.Process
import Text.Printf (printf)

class Download t where
    -- | The method used to retrieve this target.
    method :: t -> RetrieveMethod
    -- | The directory containing the target's files.  For most target types, these
    --  files could be anything, not necessarily a Debian source directory.
    getTop :: t -> FilePath
    -- | The 'revision' function constructs a string to be used as the
    -- /Revision:/ attribute of the source package information.  This
    -- is intended to characterize the build environment of the
    -- package, including some string describing the package's current
    -- revision in the revision control system, and the versions of
    -- the build dependencies that were installed when the package was
    -- build.  If the package is not in a revision control system its
    -- upstream version number is used.
    revision :: t -> String
    -- | Transform the normal package build in some way - currently the
    -- only place this is overridden is in the Proc target.
    buildWrapper :: t -> IO NominalDiffTime -> IO NominalDiffTime
    buildWrapper _ x = x
    -- | Text to include in changelog entry.
    logText :: t -> String
    -- | Given a BuildTarget and a source tree, clean all the revision control
    -- files out of that source tree.
    cleanTarget :: t -> IO ([Output], NominalDiffTime)
    cleanTarget _ = return ([], fromInteger 0)
    -- |Some targets can return a hackage version, use this to retrieve it.
    mVersion :: t -> Maybe Version
    mVersion _ = Nothing
    -- | If the download provides a tarball which we can use as the
    -- original tarball during a run of dpkg-buildpackage, this
    -- returns its path.  Note that this path may not be in the
    -- correct position for the build to use it, it might still need
    -- to be copied or linked.  See Debian.Repo.SourceTree.origTarballPath.
    origTarball :: t -> Maybe FilePath
    origTarball _ = Nothing

{-
class Download t => BuildTarget t where
    debianSourceTree :: t -> DebianSourceTree
-}
    -- ^ Return the debian source tree.  Every target must be able to return this,
    -- since this package only builds debian packages.

-- | There are many characters which will confuse make if they appear
-- in a directory name.  This turns them all into something safer.
{-
-- Use checksums instead
escapeForMake :: String -> String
escapeForMake s =
    map escape s
    where
      escape '/' = '_'		-- obviously prohibited in a directory name
      escape '(' = '_'
      escape ')' = '_'
      escape ':' = '_'
      --escape '@' = '_'
      escape '=' = '_'		-- Caused a failure in xtla
      escape c = c
-}

md5sum s = concatMap (printf "%02x" . ord) (unpack (md5 (pack s)))

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Failed to parse URI: " ++ show s)) id (parseURI s)
