module Debian.AutoBuilder.BuildTarget.DebDir where

import Control.Exception
import Control.Monad.Trans
import Debian.AutoBuilder.BuildTarget
import Debian.AutoBuilder.ParamClass (ParamClass)
import qualified Debian.AutoBuilder.ParamClass as P
import Prelude hiding (catch)
import Debian.Repo
import System.Directory
import Extra.CIO
import Debian.Shell
--import ChangeLog

-- | Get the upstream source from one location, and the debian directory from another
data DebDir = DebDir Tgt Tgt DebianSourceTree

instance Show DebDir where
    show (DebDir t q _) = "deb-dir:(" ++ show t ++ "):(" ++ show q ++ ")"

documentation = [ "deb-dir:(<target>):(<target>) - A target of this form combines two targets,"
                , "where one points to an un-debianized source tree and the other contains"
                , "a debian subdirectory." ]

instance BuildTarget DebDir where
    getTop _ (DebDir _ _ tree) = topdir tree
    cleanTarget params (DebDir (Tgt upstream) (Tgt debian) _) path =
        do cleanTarget params upstream path
           cleanTarget params debian (appendPath "/debian" path)
    revision params (DebDir (Tgt upstream) (Tgt debian) _) =
        do upstreamRev <- revision params upstream
           case upstreamRev of
             Right rev ->
                 revision params debian >>= return . either Left (\x -> Right ("deb-dir:(" ++ rev ++ "):(" ++ x ++")"))
             Left message ->
                 return . Left $ "Unimplemented: no revision method for deb-dir upstream target: " ++ message
    logText (DebDir _ _ _) revision = "deb-dir revision: " ++ maybe "none" id revision

prepareDebDir :: (ParamClass p, CIO m) => p -> Tgt -> Tgt -> m (Either String Tgt)
prepareDebDir params (Tgt upstream) (Tgt debian) = 
    liftIO  (try (createDirectoryIfMissing True (P.topDir params ++ "/deb-dir"))) >>=
    either (return . Left . show) (const copyUpstream) >>=
    either (return . Left) (const copyDebian) >>=
    either (return . Left) (const (findDebianSourceTree (rootEnvPath dest))) >>=
    either (\ message -> return $ Left ("Couldn't find source tree at " ++ show dest ++ ": " ++ message))
           (return . Right . Tgt . DebDir (Tgt upstream) (Tgt debian))
    where
      copyUpstream = runTaskAndTest (cleanStyle dest (commandTask cmd1))
      copyDebian = runTaskAndTest (cleanStyle dest (commandTask cmd2))
      upstreamDir = getTop params upstream
      debianDir = getTop params debian
      dest = P.topDir params ++ "/deb-dir/" ++ escapeForMake ("deb-dir:(" ++ show upstream ++ "):(" ++ show debian ++ ")") 
      cmd1 = ("set -x && rsync -aHxSpDt --delete '" ++ outsidePath upstreamDir ++ "/' '" ++ dest ++ "'")
      cmd2 = ("set -x && rsync -aHxSpDt --delete '" ++ outsidePath debianDir ++ "/debian' '" ++ dest ++ "/'")
      cleanStyle dest = setStart (Just (" Prepare deb-dir target in " ++ show dest))
