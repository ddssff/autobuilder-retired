{-# LANGUAGE ScopedTypeVariables #-}
-- |Modify a target so that \/proc is mounted while it builds.
module Debian.AutoBuilder.BuildTarget.Proc where

import Control.Exception (SomeException)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.BuildTarget.Temp as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.AutoBuilder.Tgt (DL)
import Debian.Repo
import System.Directory (createDirectoryIfMissing)
import System.Unix.Progress (lazyProcessEF)
import System.Unix.QIO (quieter)
--import System.Unix.Progress (qPutStrLn)

data Proc = Proc DL R.RetrieveMethod

documentation = [ "proc:<target> - A target of this form modifies another target by ensuring"
                , "that /proc is mounted during the build.  This target should only be"
                , "used if absolutely necessary, because it reveals details of the build"
                , "machine which might be different from the machine on which the package"
                , "is ultimately installed." ]

instance Download Proc where
    method (Proc _ m) = m
    getTop params (Proc s _) = getTop params s
    revision params (Proc s _) =  
        Debian.AutoBuilder.BuildTarget.Common.revision params s >>= return . ("proc:" ++)
    buildWrapper _params buildOS _buildTree _status _target action = withProc buildOS action
    logText (Proc s _) revision = logText s revision ++ " (with /proc mounted)"
    cleanTarget params (Proc s _) source = cleanTarget params s source
{-
instance BuildTarget Proc where
    debianSourceTree (Proc s _) = debianSourceTree s
-}

prepare :: P.CacheRec -> T.Download -> R.RetrieveMethod -> AptIOT IO T.Download
prepare cache base m =
    -- return $ Proc base m
    do baseRev <- liftIO $ revision (P.params cache) base
       return $ T.Download {
                    T.method' = m
                  , T.getTop = getTop (P.params cache) base
                  , T.revision = "proc:" ++ baseRev
                  , T.logText = logText base (error "Proc prepare" :: Either SomeException String) ++ " (with /proc mounted)"
                  , T.mVersion = Nothing
                  , T.origTarball = Nothing
                  , T.cleanTarget = cleanTarget (P.params cache) base
                  }

withProc :: OSImage -> IO a -> IO a
withProc buildOS task =
    do createDirectoryIfMissing True dir
       _ <- quieter (+ 1) $ lazyProcessEF "mount" ["--bind", "/proc", dir] Nothing Nothing L.empty
       result <- task
       _ <- quieter (+ 1) $ lazyProcessEF "umount" [dir] Nothing Nothing L.empty
       return result
    where
      dir = rootPath (rootDir buildOS) ++ "/proc"
