-- |Modify a target so that \/proc is mounted while it builds.
module BuildTarget.Proc where

import BuildTarget
import Linspire.Unix.Process
import Debian.IO
import Debian.SourceTree
import Debian.Types

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
        BuildTarget.revision s >>= return . maybe Nothing (Just . ("proc:" ++))
    buildPkg noClean setEnv buildOS buildTree status _ =
        do vBOL 0 >> vPutStrLn 0 "Mouting /proc during target build"
           io $ simpleProcess "mount" ["--bind", "/proc", rootPath (rootDir buildOS) ++ "/proc"] 
           result <- buildDebs noClean setEnv buildOS buildTree status
           io $ simpleProcess "umount" [rootPath (rootDir buildOS) ++ "/proc"]
           return result
    logText (Proc (Tgt s)) revision = logText s revision ++ " (with /proc mounted)"

prepareProc :: FilePath -> Bool -> Tgt -> AptIO Tgt
prepareProc _ _ base = return $ Tgt $ Proc base
