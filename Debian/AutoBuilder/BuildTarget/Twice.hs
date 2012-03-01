-- |Modify a target so that dpkg-buildpackage is run again if it fails the first time.
module Debian.AutoBuilder.BuildTarget.Twice where

import Control.Monad.Trans (liftIO)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.AutoBuilder.Tgt (DL)
import Debian.Repo (AptIOT)

data Twice = Twice DL R.RetrieveMethod

documentation = [ "twice:<target> - A target of this form modifies another target by"
                , "ensuring that dpkg-buildpackage is run a second time if it fails"
                , "the first time.  For some reason, certain packages are designed"
                , "to fail the first time to prevent fully automated builds."]

-- This is a quick and dirty implementation, if you nest this inside another
-- target type it will have no effect.
instance Download Twice where
    method (Twice _ m) = m
    getTop (Twice s _) = getTop s
    revision (Twice s _) =  
        "twice:" ++ Debian.AutoBuilder.BuildTarget.Common.revision s
    buildWrapper _ action = action >> action
    logText (Twice s _) = logText s ++ " (twice if necessary)"
    cleanTarget (Twice s _) = cleanTarget s

prepare :: P.CacheRec -> DL -> R.RetrieveMethod -> AptIOT IO (Either String Twice)
prepare _cache base m = liftIO $ return . Right $ Twice base m
