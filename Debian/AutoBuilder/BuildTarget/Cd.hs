-- |Modify a target so we cd to a subdirectory before building
module Debian.AutoBuilder.BuildTarget.Cd where

import Debian.Repo

import Debian.AutoBuilder.BuildTarget
import Debian.AutoBuilder.ParamClass (ParamClass)
import qualified Debian.AutoBuilder.ParamClass as P
import System.FilePath ((</>))
import Extra.CIO

data Cd = Cd FilePath Tgt

instance Show Cd where
    show (Cd p t) = "cd:" ++ show p ++ ":" ++ show t

documentation = [ "cd:<relpath>:<target> - A target of this form modifies another target by"
                , "changing directories into a subdirectory before doing the build.  It is"
                , "used for repositories where the debian directory is in a subdirectory."]

instance BuildTarget Cd where
    getTop params (Cd subdir (Tgt s)) = let top = getTop params s in top {envPath = envPath top </> subdir}
    cleanTarget params (Cd subdir (Tgt s)) source = cleanTarget params s (source {envPath = envPath source </> subdir})
    revision params (Cd subdir (Tgt s)) =  
        Debian.AutoBuilder.BuildTarget.revision params s >>= return . either Left (Right . (("cd:" ++ subdir ++ ":") ++))
    buildPkg params buildOS buildTree status _ =
        do vPutStrBl 0 "chdir during target build"
           buildDebs (P.noClean params) (P.setEnv params) buildOS buildTree status
           -- FIXME: we need to move the resulting deb files out of the subdirectory.
           -- Or we could only copy the subdirectory into the build environment.
    logText (Cd subdir (Tgt s)) revision = logText s revision ++ " (in subdirectory " ++ subdir ++ ")"

prepareCd :: (ParamClass p, CIO m) => p -> FilePath -> Tgt -> m (Either String Tgt)
prepareCd _params subdir target =
    -- FIXME: we should verify that the subdir contains a debian source tree
    return . Right . Tgt $ Cd subdir target