{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget
    ( retrieve
    , targetDocumentation
    ) where

import Control.Exception (SomeException, try, throw)
import Control.Monad.Trans (lift, liftIO)
import qualified Data.ByteString.Lazy as L
import Data.List (intersperse)
import qualified Debian.AutoBuilder.BuildTarget.Apt as Apt
import qualified Debian.AutoBuilder.BuildTarget.Cd as Cd
import qualified Debian.AutoBuilder.BuildTarget.Darcs as Darcs
import qualified Debian.AutoBuilder.BuildTarget.DebDir as DebDir
import qualified Debian.AutoBuilder.BuildTarget.Debianize as Debianize
import qualified Debian.AutoBuilder.BuildTarget.Dir as Dir
import qualified Debian.AutoBuilder.BuildTarget.Hackage as Hackage
import qualified Debian.AutoBuilder.BuildTarget.Hg as Hg
import qualified Debian.AutoBuilder.BuildTarget.Proc as Proc
import qualified Debian.AutoBuilder.BuildTarget.Patch as Patch
import qualified Debian.AutoBuilder.BuildTarget.Quilt as Quilt
import qualified Debian.AutoBuilder.BuildTarget.SourceDeb as SourceDeb
import qualified Debian.AutoBuilder.BuildTarget.Svn as Svn
import qualified Debian.AutoBuilder.BuildTarget.Tla as Tla
import qualified Debian.AutoBuilder.BuildTarget.Bzr as Bzr
import qualified Debian.AutoBuilder.BuildTarget.Uri as Uri
import qualified Debian.AutoBuilder.BuildTarget.Twice as Twice
import qualified Debian.AutoBuilder.Types.CacheRec as P
import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo (OSImage, rootPath, rootDir, findSourceTree, topdir)
import Debian.Repo.Monad (AptIOT)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Unix.Progress (lazyProcessEF)
import System.Unix.QIO (q12, quieter)

-- | Given a RetrieveMethod, perform the retrieval and return the result.
retrieve :: OSImage -> P.CacheRec -> P.RetrieveMethod -> [P.PackageFlag] -> AptIOT IO Download
retrieve buildOS cache spec flags =
    q12 (" " ++ show spec) $     
     case spec of
      P.Apt dist package -> Apt.prepare cache spec flags dist package
      P.Bzr string -> Bzr.prepare cache spec flags string

      P.Cd dir spec' ->
          retrieve buildOS cache spec' flags >>= \ target ->
          return $ Download { method = spec
                            , flags = flags
                            , getTop = getTop target </> dir
                            , logText = logText target ++ " (in subdirectory " ++ dir ++ ")"
                            , mVersion = Nothing
                            , origTarball = Nothing
                            , cleanTarget = cleanTarget target
                            , buildWrapper = id
                            }

      P.Darcs uri -> lift (Darcs.prepare cache spec flags uri)
      P.DebDir upstream debian ->
          do upstream' <- retrieve buildOS cache upstream flags
             debian' <- retrieve buildOS cache debian flags
             DebDir.prepare cache spec flags upstream' debian'
      P.Debianize package ->
          retrieve buildOS cache package flags >>=
          Debianize.prepare cache spec flags

      P.Dir path ->
          do tree <- lift (findSourceTree path)
             return $ T.Download { T.method = spec
                                 , T.flags = flags
                                 , T.getTop = topdir tree
                                 , T.logText =  "Built from local directory " ++ show spec
                                 , T.mVersion = Nothing
                                 , T.origTarball = Nothing
                                 , T.cleanTarget = \ _ -> return ([], 0)
                                 , T.buildWrapper = id
                                 }

      P.Hackage package -> Hackage.prepare cache spec flags package
      P.Hg string -> Hg.prepare cache spec flags string
      P.Patch base patch ->
          retrieve buildOS cache base flags >>=
          liftIO . Patch.prepare cache spec flags buildOS patch

      P.Proc spec' ->
          retrieve buildOS cache spec' flags >>= \ base ->
          return $ T.Download {
                       T.method = spec
                     , T.flags = flags
                     , T.getTop = T.getTop base
                     , T.logText = T.logText base ++ " (with /proc mounted)"
                     , T.mVersion = Nothing
                     , T.origTarball = Nothing
                     , T.cleanTarget = T.cleanTarget base
                     , T.buildWrapper = withProc buildOS
                     }
      P.Quilt base patches ->
          do base' <- retrieve buildOS cache base flags
             patches' <- retrieve buildOS cache patches flags
             Quilt.prepare cache spec flags base' patches'
      P.SourceDeb spec' ->
          retrieve buildOS cache spec' flags >>=
          SourceDeb.prepare cache spec flags
      P.Svn uri -> Svn.prepare cache spec flags uri
      P.Tla string -> Tla.prepare cache spec flags string
      P.Twice base -> retrieve buildOS cache base flags >>=
                      Twice.prepare spec flags
      P.Uri uri sum -> Uri.prepare cache spec flags uri sum

-- | Perform an IO operation with /proc mounted
withProc :: forall a. OSImage -> IO a -> IO a
withProc buildOS task =
    do createDirectoryIfMissing True dir
       _ <- quieter (+ 1) $ lazyProcessEF "mount" ["--bind", "/proc", dir] Nothing Nothing L.empty
       result <- try task :: IO (Either SomeException a)
       _ <- quieter (+ 1) $ lazyProcessEF "umount" [dir] Nothing Nothing L.empty
       either throw return result
    where
      dir = rootPath (rootDir buildOS) ++ "/proc"

targetDocumentation :: String
targetDocumentation =
    "TARGET TYPES\n\nEach argument to --target describes a technique for obtaining\n" ++
    "the source code used to build a target.  The following target types are available:\n\n" ++
    concat (intersperse "\n\n" $
            map (concat . intersperse "\n  ")
            [ [ "dir:<path> - A target of this form simply uses whatever it finds on"
              , "the local machine at the given path as the debian source tree."
              , "Packages built using this targets are not allowed to be uploaded"
              , "since they include no revision control information." ]
            , Apt.documentation
            , Cd.documentation
            , Darcs.documentation
            , DebDir.documentation
            , Debianize.documentation
            , Hackage.documentation
            , Hg.documentation
            , Proc.documentation
            , Quilt.documentation
            , SourceDeb.documentation
            , Svn.documentation
            , Tla.documentation
            , Uri.documentation ])
