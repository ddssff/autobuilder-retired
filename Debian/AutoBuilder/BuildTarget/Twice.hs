-- |Modify a target so that dpkg-buildpackage is run again if it fails the first time.
module Debian.AutoBuilder.BuildTarget.Twice where

import Debian.Repo

import Debian.AutoBuilder.BuildTarget
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Tgt (Tgt(Tgt))

data Twice = Twice Tgt

instance Show Twice where
    show (Twice t) = "twice:" ++ show t

documentation = [ "twice:<target> - A target of this form modifies another target by"
                , "ensuring that dpkg-buildpackage is run a second time if it fails"
                , "the first time.  For some reason, certain packages are designed"
                , "to fail the first time to prevent fully automated builds."]

instance BuildTarget Twice where
    getTop params (Twice (Tgt s)) = getTop params s
    cleanTarget params (Twice (Tgt s)) source = cleanTarget params s source
    revision params (Twice (Tgt s)) =  
        Debian.AutoBuilder.BuildTarget.revision params s >>= return . ("twice:" ++)
    -- This is a quick and dirty implementation, if you nest this inside another
    -- target type it will have no effect.
    buildPkg params buildOS buildTree status _ =
        buildDebs (P.noClean params) True (P.setEnv params) buildOS buildTree status
    logText (Twice (Tgt s)) revision = logText s revision ++ " (twice if necessary)"

prepareTwice :: P.CacheRec -> Tgt -> IO (Either String Tgt)
prepareTwice _cache base = return . Right . Tgt $ Twice base
