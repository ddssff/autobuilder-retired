-- |Modify a target so that dpkg-buildpackage is run again if it fails the first time.
module Debian.AutoBuilder.BuildTarget.Twice where

import Control.Monad.Trans (liftIO)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.AutoBuilder.Tgt (Tgt)
import Debian.Repo (AptIOT)

data Twice = Twice Tgt R.RetrieveMethod

instance Show Twice where
    show (Twice t _) = "twice:" ++ show t

documentation = [ "twice:<target> - A target of this form modifies another target by"
                , "ensuring that dpkg-buildpackage is run a second time if it fails"
                , "the first time.  For some reason, certain packages are designed"
                , "to fail the first time to prevent fully automated builds."]

instance BuildTarget Twice where
    method (Twice _ m) = m
    getTop params (Twice s _) = getTop params s
    cleanTarget params (Twice s _) source = cleanTarget params s source
    revision params (Twice s _) =  
        Debian.AutoBuilder.BuildTarget.Common.revision params s >>= return . ("twice:" ++)
    -- This is a quick and dirty implementation, if you nest this inside another
    -- target type it will have no effect.
    buildWrapper _ _ _ _ _ action = action >> action
    logText (Twice s _) revision = logText s revision ++ " (twice if necessary)"

prepare :: P.CacheRec -> Tgt -> R.RetrieveMethod -> AptIOT IO (Either String Twice)
prepare _cache base m = liftIO $ return . Right $ Twice base m
