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
module Debian.AutoBuilder.BuildTarget 
    ( BuildTarget(..)
    , Dir(Dir)
    , Build(Build)
    , md5sum
    ) where

import Control.Exception (Exception)
import Data.Time (NominalDiffTime)
import Debian.Repo
import Debian.AutoBuilder.ParamClass (ParamClass)
import qualified Debian.AutoBuilder.ParamClass as P
import Debian.Version(DebianVersion)
import System.Unix.Process

import Data.Char (ord)
import Text.Printf (printf)
import Happstack.Crypto.MD5 (md5)
import Data.ByteString.Lazy.Char8 (pack, unpack)

-- | BuildTarget represents the type class of methods for obtaining a
-- SourceTree: tla, apt, darcs, etc.
class BuildTarget t where
    -- | The directory containing the target's files.  For most target types, these
    --  files could be anything, not necessarily a Debian source directory.
    getTop :: ParamClass p => p -> t -> FilePath
    -- | Given a BuildTarget and a source tree, clean all the revision control
    -- files out of that source tree.
    cleanTarget :: (ParamClass p) => p -> t -> FilePath -> IO ([Output], NominalDiffTime)
    cleanTarget _ _ _ = return ([], fromInteger 0)
    -- | The 'revision' function constructs a string to be used as the
    -- /Revision:/ attribute of the source package information.  This
    -- is intended to characterize the build environment of the
    -- package, including some string describing the package's current
    -- revision in the revision control system, and the versions of
    -- the build dependencies that were installed when the package was
    -- build.  If the package is not in a revision control system its
    -- upstream version number is used.
    revision :: (ParamClass p) => p -> t -> IO String
    -- |Default function to build the package for this target.
    -- Currently this is only overridden by the proc: target which
    -- mounts /proc, then calls buildDebs, then unmounts /proc.
    buildPkg :: (ParamClass p) => p -> OSImage -> DebianBuildTree -> SourcePackageStatus -> t -> IO NominalDiffTime
    buildPkg params buildOS buildTree status _target =
        buildDebs (P.noClean params) False (P.setEnv params) buildOS buildTree status
    -- | Text to include in changelog entry.
    logText :: Exception e => t -> Either e String -> String
    -- |Some targets can return a debian version, use this to retrieve it.
    mVersion :: t -> Maybe DebianVersion
    mVersion _ = Nothing

-- |Dir is a simple instance of BuildTarget representing building the
-- debian source in a local directory.  This type of target is used
-- for testing, and is also returned by the clean method when the
-- source control information has been stripped out of some other type
-- of BuildTarget.
data Dir = Dir SourceTree

instance Show Dir where
    show (Dir tree) = "dir:" ++ topdir tree

instance BuildTarget Dir where
    getTop _ (Dir tree) = topdir tree
    revision _ (Dir _) = fail "Dir targets do not have revision strings"
    logText (Dir tree) _ = "Built from local directory " ++ topdir tree

-- |Build is similar to Dir, except that it owns the parent directory
-- of the source directory.  This is required for building packages
-- because all of the debs, tarballs etc appear in the parent directory.
data Build = Build DebianBuildTree

instance Show Build where
    show (Build tree) = "build:" ++ topdir tree

instance BuildTarget Build where
    getTop _ (Build tree) = topdir tree
    revision _ (Build _) = fail "Build targets do not have revision strings"
    logText (Build tree) _ = "Built from local directory " ++ topdir tree

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
