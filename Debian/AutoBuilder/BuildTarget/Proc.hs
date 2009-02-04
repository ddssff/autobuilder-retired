-- |Modify a target so that \/proc is mounted while it builds.
module Debian.AutoBuilder.BuildTarget.Proc where

import Debian.Repo

import Debian.AutoBuilder.BuildTarget
import Debian.AutoBuilder.ParamClass (RunClass)
import qualified Debian.AutoBuilder.ParamClass as P
import System.Unix.Process
import Extra.CIO
import Control.Monad.Trans

data Proc = Proc Tgt

instance Show Proc where
    show (Proc t) = "proc:" ++ show t

documentation = [ "proc:<target> - A target of this form modifies another target by ensuring"
                , "that /proc is mounted during the build.  This target should only be"
                , "used if absolutely necessary, because it reveals details of the build"
                , "machine which might be different from the machine on which the package"
                , "is ultimately installed." ]

instance BuildTarget Proc where
    getTop params (Proc (Tgt s)) = getTop params s
    cleanTarget params (Proc (Tgt s)) source = cleanTarget params s source
    revision params (Proc (Tgt s)) =  
        Debian.AutoBuilder.BuildTarget.revision params s >>= return . either Left (Right . ("proc:" ++))
    buildPkg params buildOS buildTree status _ =
        do vPutStrBl 0 "Mouting /proc during target build"
           liftIO $ simpleProcess "mount" ["--bind", "/proc", rootPath (rootDir buildOS) ++ "/proc"] 
           result <- buildDebs (P.noClean params) (P.setEnv params) buildOS buildTree status
           liftIO $ simpleProcess "umount" [rootPath (rootDir buildOS) ++ "/proc"]
           return result
    logText (Proc (Tgt s)) revision = logText s revision ++ " (with /proc mounted)"

prepareProc :: (RunClass p, CIO m) => p -> Tgt -> m (Either String Tgt)
prepareProc _ base = return . Right . Tgt $ Proc base
