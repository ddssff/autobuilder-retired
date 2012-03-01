module Debian.AutoBuilder.BuildTarget.Dir where

import Control.Monad.Trans (lift)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Repo

-- |Dir is a simple instance of BuildTarget representing building the
-- debian source in a local directory.  This type of target is used
-- for testing, and is also returned by the clean method when the
-- source control information has been stripped out of some other type
-- of BuildTarget.
{-
data Dir = Dir SourceTree R.RetrieveMethod

instance Download Dir where
    method (Dir _ m) = m
    getTop (Dir tree _) = topdir tree
    revision (Dir _ _) = fail "Dir targets do not have revision strings"
    logText (Dir tree _) = "Built from local directory " ++ topdir tree
-}

-- |Build is similar to Dir, except that it owns the parent directory
-- of the source directory.  This is required for building packages
-- because all of the debs, tarballs etc appear in the parent directory.
{-
data Build = Build DebianBuildTree R.RetrieveMethod

instance Download Build where
    method (Build _ m) = m
    getTop (Build tree _) = topdir tree
    revision (Build _ _) = fail "Build targets do not have revision strings"
    logText (Build tree _) = "Built from local directory " ++ topdir tree
-}

-- |Prepare a Dir target
prepare :: P.CacheRec -> FilePath -> R.RetrieveMethod -> AptIOT IO T.Download
prepare _cache path m =
    do tree <- lift (findSourceTree path)
       return $ T.Download { T.method = m
                           , T.getTop = topdir tree
                           , T.revision = fail "Build targets do not have revision strings"
                           , T.logText =  "Built from local directory " ++ topdir tree
                           , T.mVersion = Nothing
                           , T.origTarball = Nothing
                           , T.cleanTarget = \ _ -> return ([], 0)
                           , T.buildWrapper = id
                           }
