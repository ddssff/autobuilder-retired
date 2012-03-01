{-# LANGUAGE ScopedTypeVariables #-}
-- |Modify a target so that \/proc is mounted while it builds.
module Debian.AutoBuilder.BuildTarget.Proc where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Repo
import System.Directory (createDirectoryIfMissing)
import System.Unix.Progress (lazyProcessEF)
import System.Unix.QIO (quieter)

documentation = [ "proc:<target> - A target of this form modifies another target by ensuring"
                , "that /proc is mounted during the build.  This target should only be"
                , "used if absolutely necessary, because it reveals details of the build"
                , "machine which might be different from the machine on which the package"
                , "is ultimately installed." ]

prepare :: P.CacheRec -> T.Download -> R.RetrieveMethod -> OSImage -> AptIOT IO T.Download
prepare _cache base m buildOS =
    return $ T.Download {
                 T.method = m
               , T.getTop = T.getTop base
               , T.revision = "proc:" ++ T.revision base
               , T.logText = T.logText base ++ " (with /proc mounted)"
               , T.mVersion = Nothing
               , T.origTarball = Nothing
               , T.cleanTarget = T.cleanTarget base
               , T.buildWrapper = withProc buildOS
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
