-- |Modify a target so that \/proc is mounted while it builds.
module Debian.AutoBuilder.BuildTarget.Proc where

import qualified Data.ByteString.Lazy.Char8 as L
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Tgt (Tgt)
import Debian.Repo
import System.Directory (createDirectoryIfMissing)
import System.Unix.Progress (lazyProcessEF)
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
    getTop params (Proc s) = getTop params s
    cleanTarget params (Proc s) source = cleanTarget params s source
    revision params (Proc s) =  
        Debian.AutoBuilder.BuildTarget.Common.revision params s >>= return . ("proc:" ++)
    buildWrapper _params buildOS _buildTree _status _target action = withProc buildOS action
    logText (Proc s) revision = logText s revision ++ " (with /proc mounted)"

prepare :: P.CacheRec -> Tgt -> AptIOT IO Proc
prepare _cache base = return $ Proc base

withProc :: OSImage -> IO a -> IO a
withProc buildOS task =
    do createDirectoryIfMissing True dir
       _ <- lazyProcessEF "mount" ["--bind", "/proc", dir] Nothing Nothing L.empty
       result <- task
       _ <- lazyProcessEF "umount" [dir] Nothing Nothing L.empty
       return result
    where
      dir = rootPath (rootDir buildOS) ++ "/proc"
