{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, FlexibleContexts #-}
-- |Various ways of obtaining a Debian source code tree.
--
-- * "BuildTarget.Tla"
--
-- * "BuildTarget.Svn"
--
-- * "BuildTarget.SourceDeb"
--
-- * "BuildTarget.Hg"
--
-- * "BuildTarget.DebDir"
--
-- * "BuildTarget.Darcs"
--
-- * "BuildTarget.Apt"
--
-- * "BuildTarget.Proc"
--
-- * "BuildTarget.Uri"
--
-- * "BuildTarget.Quilt"
--
module BuildTarget 
    ( BuildTarget(..)
    , Tgt(Tgt)
    , Dir(Dir)
    , Build(Build)
    , prepareDir
    , escapeForMake
    ) where

import Extra.TIO
import Debian.Types.SourceTree
import Debian.SourceTree
import Debian.Types
import Control.Monad
import Data.Maybe
import System.Time
import System.Unix.Process

-- | Objects of type Tgt contain an instance of the BuildTarget type
-- class.
data Tgt = forall a. (Show a, BuildTarget a) => Tgt a

instance Show Tgt where
    show (Tgt a) = show a

--getSourceTree' :: Tgt -> SourceTree
--getSourceTree' (Tgt a) = getSourceTree a

-- | BuildTarget represents the type class of methods for obtaining a
-- SourceTree: tla, apt, darcs, etc.
class BuildTarget t where
    -- | The directory containing the target's files.  For most target types, these
    --  files could be anything, not necessarily a Debian source directory.
    getTop :: BuildTarget t => t -> EnvPath
    -- | Given a BuildTarget and a source tree, clean all the revision control
    -- files out of that source tree.
    cleanTarget :: BuildTarget t => t -> EnvPath -> TIO (Either String ([Output], TimeDiff))
    cleanTarget _ _ = return . Right $ ([], noTimeDiff)
    -- | The 'revision' function constructs a string to be used as the
    -- /Revision:/ attribute of the source package information.  This
    -- is intended to characterize the build environment of the
    -- package, including some string describing the package's current
    -- revision in the revision control system, and the versions of
    -- the build dependencies that were installed when the package was
    -- build.  If the package is not in a revision control system its
    -- upstream version number is used.
    revision :: t -> TIO (Either String String)
    -- |Copy 
    prepareCopy :: (BuildTarget t) => t -> DebianBuildTree -> EnvPath -> TIO (Either String DebianBuildTree)
    prepareCopy _target buildTree dest = copyDebianBuildTree buildTree dest

    -- | Prepare a source tree for an actual build, run the function to do
    -- the build, and then tear down any preparations we just made.
{-
    withPreparedSource :: (BuildTarget t) => (DebianBuildTree -> AptIO ()) -> t -> DebianBuildTree -> AptIO ()
    withPreparedSource f tgt path tree = f path tree
-}

    -- |Default function to build the package for this target.
    buildPkg :: BuildTarget t => Bool -> [String] -> OSImage -> DebianBuildTree -> SourcePackageStatus -> t -> TIO (Either String TimeDiff)
    buildPkg noClean setEnv buildOS buildTree status _target =
        buildDebs noClean setEnv buildOS buildTree status
    -- | Text to include in changelog entry.
    logText :: t -> Maybe String -> String

-- |Dir is a simple instance of BuildTarget representing building the
-- debian source in a local directory.  This type of target is used
-- for testing, and is also returned by the clean method when the
-- source control information has been stripped out of some other type
-- of BuildTarget.
data Dir = Dir SourceTree

instance Show Dir where
    show (Dir tree) = "dir:" ++ outsidePath (topdir tree)

instance BuildTarget Dir where
    getTop (Dir tree) = topdir tree
    revision (Dir _) = return (Left "Dir targets do not have revision strings")
    logText (Dir tree) _ = "Built from local directory " ++ outsidePath (topdir tree)

-- |Prepare a Dir target
prepareDir :: Bool -> FilePath -> Bool -> EnvPath -> TIO (Either String Tgt)
prepareDir _ _ _ path =
    findSourceTree path >>=
    return . either (\ message -> Left $ "No source tree at " ++ outsidePath path ++ ": " ++ message) (Right . Tgt . Dir)

-- |Build is similar to Dir, except that it owns the parent directory
-- of the source directory.  This is required for building packages
-- because all of the debs, tarballs etc appear in the parent directory.
data Build = Build DebianBuildTree

instance Show Build where
    show (Build tree) = "build:" ++ outsidePath (topdir tree)

instance BuildTarget Build where
    getTop (Build tree) = topdir tree
    revision (Build _) = return (Left "Build targets do not have revision strings")
    logText (Build tree) _ = "Built from local directory " ++ outsidePath (topdir tree)

-- | There are many characters which will confuse make if they appear
-- in a directory name.  This turns them all into something safer.
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
