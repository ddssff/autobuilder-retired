{-# LANGUAGE ExistentialQuantification #-}
-- |Modify a target so we cd to a subdirectory before building
module Debian.AutoBuilder.BuildTarget.Cd where

import qualified Debian.AutoBuilder.BuildTarget.Common as C
import qualified Debian.AutoBuilder.BuildTarget.Temp as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Repo.Monad (AptIOT)
import System.FilePath ((</>))

documentation = [ "cd:<relpath>:<target> - A target of this form modifies another target by"
                , "changing directories into a subdirectory before doing the build.  It is"
                , "used for repositories where the debian directory is in a subdirectory."]

{-
data Cd = forall a. Download a => Cd FilePath a R.RetrieveMethod

instance Download Cd where
    method (Cd _ _ m) = m
    getTop params (Cd subdir t _) = let top = getTop params t in top </> subdir
    revision params (Cd subdir t _) =  Debian.AutoBuilder.BuildTarget.Common.revision params t >>= return . (("cd:" ++ subdir ++ ":") ++)
    logText (Cd subdir t _) revision = logText t revision ++ " (in subdirectory " ++ subdir ++ ")"
    cleanTarget params (Cd subdir t _) source = cleanTarget params t (source </> subdir)
-}

prepare :: P.CacheRec -> FilePath -> T.Download -> R.RetrieveMethod -> AptIOT IO T.Download
prepare _cache subdir target m =
    -- FIXME: we should verify that the subdir contains a debian source tree
    -- return $ Cd subdir target m
    do     
    return $ T.Download { T.method = m
                        , T.getTop = C.getTop target </> subdir
                        , T.revision = "cd:" ++ subdir ++ ":" ++ C.revision target
                        , T.logText = T.logText target ++ " (in subdirectory " ++ subdir ++ ")"
                        , T.mVersion = Nothing
                        , T.origTarball = Nothing
                        , T.cleanTarget = T.cleanTarget target
                        , T.buildWrapper = id
                        } 
