-- |Modify a target so that \/proc is mounted while it builds.
module BuildTarget.Proc where

import BuildTarget
import System.Unix.Process
import Extra.TIO
import Debian.SourceTree
import Debian.Types
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
    getTop (Proc (Tgt s)) = getTop s
    cleanTarget (Proc (Tgt s)) source = cleanTarget s source
    revision (Proc (Tgt s)) =  
        BuildTarget.revision s >>= return . either Left (Right . ("proc:" ++))
    buildPkg noClean setEnv buildOS buildTree status _ =
        do vPutStrBl 0 "Mouting /proc during target build"
           lift $ simpleProcess "mount" ["--bind", "/proc", rootPath (rootDir buildOS) ++ "/proc"] 
           result <- buildDebs noClean setEnv buildOS buildTree status
           lift $ simpleProcess "umount" [rootPath (rootDir buildOS) ++ "/proc"]
           return result
    logText (Proc (Tgt s)) revision = logText s revision ++ " (with /proc mounted)"

prepareProc :: FilePath -> Bool -> Tgt -> TIO (Either String Tgt)
prepareProc _ _ base = return . Right . Tgt $ Proc base
