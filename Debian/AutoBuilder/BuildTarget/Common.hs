{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, FlexibleContexts #-}
-- |Various ways of obtaining a Debian source code tree.  Here is
-- a list giving the syntax of the supported target types:
--
-- [apt:\<distribution\>:\<packagename\>, @apt:\<distribution\>:\<packagename\>=\<version\>@] - a target of this form looks up
--                the sources.list named @\<distribution\>@ and retrieves the package with
--                the given name from that distribution.
--
-- [@cd:\<relpath\>:\<target\>@] - A target of this form modifies another target by
--                changing directories into a subdirectory before doing the build.  It is
--                used for repositories where the debian directory is in a subdirectory.
--
-- [@darcs:\<string\>@] - a target of this form obtains the source code by running
--                darcs get @\<string\>@.  If the argument needs to use ssh to reach the darcs
--                repository, it is necessary to set up ssh keys to allow access without
--                typing a password.  See 'Debian.AutoBuilder.ParamClass.doSSHExport' for help doing this.
--
-- [@deb-dir:(\<target\>):(\<target\>)@] - A target of this form combines two targets,
--                where one points to an un-debianized source tree and the other contains
--                a debian subdirectory.
--
-- [@dir:\<path\>@] - A target of this form simply uses whatever it finds on
--              the local machine at the given path as the debian source tree.
--              Packages built using this targets are not allowed to be uploaded
--              since they include no revision control information.
-- 
-- [@hackage:\<uri\>, hackage:\<uri\>=\<version\>@] - Similar to the URI target, this
--                target downloads and unpacks source code from the haskell hackage
--                repository.  Without the version number the newest version is retrieved.
-- 
-- [@hg:\<string\>@] - A target of this form target obtains the source
--                code by running the Mercurial command @hg clone \<string\>@.
--
-- [@proc:\<target\>@] - A target of this form modifies another target by ensuring
--                that @/proc@ is mounted during the build.  This target should only be
--                used if absolutely necessary, because it reveals details of the build
--                machine which might be different from the machine on which the package
--                is ultimately installed.
--
-- [@quilt:(\<target1\>):(\<target2\>)@] - In a target of this form, target1 is
--                any source tree, and target2 is a quilt directory which contains
--                a debian style changelog file named @changelog@, a file named
--                @series@ with a list of patch file names, and finally the patch
--                files listed in the series file.  The quilt system is used to apply
--                the patches to the source tree before building.
--
-- [@sourcedeb:\<target\>@] - A target of this form unpacks the source deb
--                retrieved by the original target and presents an unpacked source
--                tree for building.  Thus, the original target should retrieve a
--                directory containing a @.dsc@ file, a @.tar.gz@, and an optional
--                @.diff.gz@ file.
--
-- [@svn:\<uri\>@] - A target of this form retrieves the source code from
--                a subversion repository.
--
-- [@tla:\<revision\>@] - A target of this form retrieves the a TLA archive with the
--                given revision name.
--
-- [@uri:\<string\>:\<md5sum\>@] - A target of this form retrieves the file at the
--                given URI, which is assumed to be a gzipped tarball.  The optional md5sum
--                suffix causes the build to fail if the downloaded file does not match
--                this checksum.  This prevents builds when the remote tarball has changed.
module Debian.AutoBuilder.BuildTarget.Common
    ( Download(..)
    , md5sum
    , mustParseURI
    ) where

import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Char (ord)
import Data.Time (NominalDiffTime)
import Data.Version (Version)
--import Debian.Repo
--import qualified Debian.AutoBuilder.Types.CacheRec as P
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

-- | BuildTarget represents the type class of methods for obtaining a
-- SourceTree: tla, apt, darcs, etc.
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
