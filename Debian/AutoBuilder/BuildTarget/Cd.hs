-- |Modify a target so we cd to a subdirectory before building
module Debian.AutoBuilder.BuildTarget.Cd where

import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Tgt (Tgt)
import Debian.Repo.Monad (AptIOT)
import System.FilePath ((</>))

data Cd = Cd FilePath Tgt

instance Show Cd where
    show (Cd p t) = "cd:" ++ p ++ ":" ++ show t

documentation = [ "cd:<relpath>:<target> - A target of this form modifies another target by"
                , "changing directories into a subdirectory before doing the build.  It is"
                , "used for repositories where the debian directory is in a subdirectory."]

instance BuildTarget Cd where
    getTop params (Cd subdir t) = let top = getTop params t in top </> subdir
    cleanTarget params (Cd subdir t) source = cleanTarget params t (source </> subdir)
    revision params (Cd subdir t) =  Debian.AutoBuilder.BuildTarget.Common.revision params t >>= return . (("cd:" ++ subdir ++ ":") ++)
    logText (Cd subdir t) revision = logText t revision ++ " (in subdirectory " ++ subdir ++ ")"

prepare :: P.CacheRec -> FilePath -> Tgt -> AptIOT IO Cd
prepare _cache subdir target =
    -- FIXME: we should verify that the subdir contains a debian source tree
    return $ Cd subdir target
