-- |Modify a target so that \/proc is mounted while it builds.
module BuildTarget.Proc where

import BuildTarget
import Linspire.Unix.Process
import Debian.IO
import Debian.OSImage
import Debian.SourceTree
import Debian.Types

data Proc = Proc Tgt

instance Show Proc where
    show (Proc t) = "proc:" ++ show t

instance BuildTarget Proc where
    getTop (Proc (Tgt s)) = getTop s
    cleanTarget (Proc (Tgt s)) source = cleanTarget s source
    revision (Proc (Tgt s)) =  
        BuildTarget.revision s >>= return . maybe Nothing (Just . ("proc:" ++))
    buildPkg noClean setEnv buildOS buildTree status _ =
        do vBOL 0 >> vPutStrLn 0 "Mouting /proc during target build"
           io $ simpleProcess "mount" ["--bind", "/proc", rootPath (osRoot buildOS) ++ "/proc"] 
           result <- buildDebs noClean setEnv buildOS buildTree status
           io $ simpleProcess "umount" [rootPath (osRoot buildOS) ++ "/proc"]
           return result
    logText (Proc (Tgt s)) revision = logText s revision ++ " (with /proc mounted)"

prepareProc :: FilePath -> Bool -> Tgt -> AptIO Tgt
prepareProc _ _ base = return $ Tgt $ Proc base
