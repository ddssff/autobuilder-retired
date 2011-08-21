module Debian.AutoBuilder.BuildTarget.Dir where

import Control.Monad.Trans (lift)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Params as P
import Debian.Repo

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

-- |Prepare a Dir target
prepare :: P.CacheRec -> FilePath -> AptIOT IO Dir
prepare _cache path = lift (findSourceTree path) >>= return . Dir
