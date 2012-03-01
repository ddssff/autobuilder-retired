-- |Modify a target so that dpkg-buildpackage is run again if it fails the first time.
module Debian.AutoBuilder.BuildTarget.Twice where

import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.BuildTarget.Temp as T
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Repo (AptIOT)

documentation = [ "twice:<target> - A target of this form modifies another target by"
                , "ensuring that dpkg-buildpackage is run a second time if it fails"
                , "the first time.  For some reason, certain packages are designed"
                , "to fail the first time to prevent fully automated builds."]

prepare :: T.Download -> R.RetrieveMethod -> AptIOT IO T.Download
prepare base m =
    do return $ T.Download {
                    T.method = m
                  , T.getTop = getTop base
                  , T.revision = "twice:" ++ revision base
                  , T.logText = logText base ++ " (twice if necessary)"
                  , T.mVersion = Nothing
                  , T.origTarball = Nothing
                  , T.cleanTarget = T.cleanTarget base
                  -- This is a quick and dirty implementation, if you nest this inside another
                  -- target type it will have no effect.
                  , T.buildWrapper = \ action -> action >> action
                  }
