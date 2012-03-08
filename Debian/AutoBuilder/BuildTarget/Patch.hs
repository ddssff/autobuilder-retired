-- | Copy the target and apply a patch.
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Patch where

import Control.Exception (try)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo (OSImage, findSourceTree, copySourceTree, SourceTree(dir'))
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>))
import System.Unix.Progress (lazyProcessE)
import System.Unix.Process (collectOutputUnpacked)

{-
-- |Scan the flag list for Patch flag, and apply the patches
patch :: P.CacheRec -> [P.PackageFlag] -> String -> Version -> IO ()
patch cache flags name version =
    mapM_ patch' flags
    where
      patch' :: P.PackageFlag -> IO ()
      patch' (P.Patch text) =
          do (_out, err, res) <- lazyProcessE "/usr/bin/patch" ["-p1"] (Just (unpacked cache name version)) Nothing text >>=
                                 return . collectOutputUnpacked
             case res of
               ExitFailure n -> error ("patch " ++ show (unpacked cache name version) ++ " -> " ++
                                       show n ++ "\noutput: " ++ err ++ "\npatch:\n" ++ B.unpack text)
               ExitSuccess -> return ()
      patch' _ = return ()

instance Show Patch where
    show (Patch t) = "patch:" ++ show t
-}

documentation = [ "Patch <target> <patchtext> - Apply the patch to the target." ]

prepare :: P.CacheRec -> P.RetrieveMethod -> [P.PackageFlag] -> OSImage -> String -> T.Download -> IO T.Download
prepare cache m flags buildOS patch base =
    do baseTree <- findSourceTree (T.getTop base)
       liftIO (createDirectoryIfMissing True copyDir)
       tree <- copySourceTree baseTree copyDir
       (_out, err, res) <- lazyProcessE "/usr/bin/patch" ["-p1"] (Just copyDir) Nothing (B.pack patch) >>=
                           return . collectOutputUnpacked
       case res of
         ExitFailure n -> error ("patch " ++ show copyDir ++ " -> " ++
                                 show n ++ "\noutput: " ++ err ++ "\npatch:\n" ++ indent patch)
         ExitSuccess ->
             return $ T.Download {
                          T.method = m
                        , T.flags = flags
                        , T.getTop = dir' tree
                        , T.logText = T.logText base ++ " (with patch applied)"
                        , T.mVersion = Nothing
                        , T.origTarball = Nothing
                        , T.cleanTarget = T.cleanTarget base
                        , T.buildWrapper = id
                        }
    where
      copyDir = P.topDir cache ++ "/quilt/" ++ show (md5 (B.pack (show m)))

indent = unlines . map ("  " ++) . lines

{-
instance BuildTarget Patch where
    getTop params (Patch t _) = getTop params t
    cleanTarget params (Patch t _) source = cleanTarget params t source
    -- We can't include the whole patch text in the revision string.
    revision params (Patch t _) =  
        Debian.AutoBuilder.BuildTarget.Common.revision params t >>= return . ("patch:" ++)
    buildWrapper _params buildOS _buildTree _status _target action = withProc buildOS action
    logText (Proc s) revision = logText s revision ++ " (with /proc mounted)"

prepare :: P.CacheRec -> Tgt -> String -> AptIOT IO Patch
prepare cache base patch = return $ Patch base patch

-- |Scan the flag list for Patch flag, and apply the patches
patch :: FilePath -> Patch [P.PackageFlag] -> String -> DebianVersion -> IO ()
patch top flags name version =
    mapM_ patch' flags
    where
      patch' :: P.PackageFlag -> IO ()
      patch' (P.Patch text) =
          do (_out, err, res) <- lazyProcessE "/usr/bin/patch" ["-p1"] (Just (unpacked top name version)) Nothing text >>=
                                 return . collectOutputUnpacked
             case res of
               ExitFailure n -> error ("Applying patch to " ++ unpacked top name version ++ " -> " ++
                                       show n ++ "\noutput: " ++ err ++ "\npatch:\n" ++ B.unpack text)
               ExitSuccess -> return ()
      patch' _ = return ()
-}
