-- |Modify a target so that \/proc is mounted while it builds.
module Debian.AutoBuilder.BuildTarget.Proc where

--import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (intercalate)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Tgt (Tgt(Tgt))
import Debian.Repo
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import System.Process (rawSystem)
import System.Unix.Directory (unmountRecursiveSafely)
import System.Unix.Process
--import System.Unix.Progress (qPutStrLn)

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
        Debian.AutoBuilder.BuildTarget.Common.revision params s >>= return . ("proc:" ++)
    buildWrapper _params buildOS _buildTree _status _target action = withProc buildOS action
{-
    buildPkg params buildOS buildTree status _ =
        do hPutStrLn stderr "Mounting /proc during target build"
           withProc buildOS $ buildDebs (P.noClean params) False (P.setEnv params) buildOS buildTree status
-}
    logText (Proc (Tgt s)) revision = logText s revision ++ " (with /proc mounted)"

prepare :: P.CacheRec -> Tgt -> AptIOT IO Proc
prepare _cache base = return $ Proc base

withProc :: OSImage -> IO a -> IO a
withProc buildOS task =
    do hPutStrLn stderr "Mounting /proc during target build"
       createDirectoryIfMissing True dir
       code <- rawSystem cmd args
       case code of
         ExitSuccess ->
             do hPutStrLn stderr "Mounted /proc"
                result <- task
                hPutStrLn stderr "Unmounting /proc..."
                unmountRecursiveSafely dir
                hPutStrLn stderr "Unmounted /proc."
                return result
         _ -> do hPutStrLn stderr "Mount of /proc failed!"
                 fail (intercalate " " (cmd : args) ++ " -> " ++ show code)
    where
      cmd = "mount"
      args = ["--bind", "/proc", dir]
      dir = rootPath (rootDir buildOS) ++ "/proc"
