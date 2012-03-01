{-# LANGUAGE ExistentialQuantification #-}
-- |Modify a target so we cd to a subdirectory before building
module Debian.AutoBuilder.BuildTarget.Cd where

import qualified Debian.AutoBuilder.BuildTarget.Temp as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Repo.Monad (AptIOT)
import System.FilePath ((</>))

documentation = [ "cd:<relpath>:<target> - A target of this form modifies another target by"
                , "changing directories into a subdirectory before doing the build.  It is"
                , "used for repositories where the debian directory is in a subdirectory."]

prepare :: P.CacheRec -> FilePath -> T.Download -> R.RetrieveMethod -> AptIOT IO T.Download
prepare _cache subdir target m =
    do     
    return $ T.Download { T.method = m
                        , T.getTop = T.getTop target </> subdir
                        , T.revision = "cd:" ++ subdir ++ ":" ++ T.revision target
                        , T.logText = T.logText target ++ " (in subdirectory " ++ subdir ++ ")"
                        , T.mVersion = Nothing
                        , T.origTarball = Nothing
                        , T.cleanTarget = T.cleanTarget target
                        , T.buildWrapper = id
                        } 
