-- |Modify a target so that \/proc is mounted while it builds.
module Debian.AutoBuilder.BuildTarget.Proc where

import Control.Applicative.Error (Failing(..))
import Data.List (intercalate)
import Debian.AutoBuilder.BuildTarget
import Debian.AutoBuilder.ParamClass (RunClass)
import qualified Debian.AutoBuilder.ParamClass as P
import Debian.Extra.CIO
import Debian.Repo
import System.Process (rawSystem)
import System.Unix.Process

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
        Debian.AutoBuilder.BuildTarget.revision params s >>= return . ("proc:" ++)
    buildPkg params buildOS buildTree status _ =
        do vPutStrBl 0 "Mouting /proc during target build"
           code <- rawSystem "mount" ["--bind", "/proc", rootPath (rootDir buildOS) ++ "/proc"]
           case code of
             ExitSuccess ->
                 do result <- buildDebs (P.noClean params) False (P.setEnv params) buildOS buildTree status
                    (out, err, code) <- simpleProcess "umount" [rootPath (rootDir buildOS) ++ "/proc"]
                    case code of
                      ExitSuccess -> return result
                      _ -> fail $ intercalate " " ("umount" : [rootPath (rootDir buildOS) ++ "/proc"]) ++ " -> " ++ show code ++ "\n\n" ++ show out ++ "\n\n" ++ show err
             _ -> fail (intercalate " " ("mount" : ["--bind", "/proc", rootPath (rootDir buildOS) ++ "/proc"]) ++ " -> " ++ show code)
    logText (Proc (Tgt s)) revision = logText s revision ++ " (with /proc mounted)"

prepareProc :: (RunClass p) => p -> Tgt -> IO Tgt
prepareProc _ base = return . Tgt $ Proc base
