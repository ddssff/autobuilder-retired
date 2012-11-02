{-# LANGUAGE ScopedTypeVariables #-}
-- |A 'uri:' target is an URI that returns a tarball, with an optional
-- md5sum if we want to ensure against the tarball changing unexpectedly.
module Debian.AutoBuilder.BuildTarget.Uri
    ( documentation
    , prepare
    , tarball
    , sourceDir
    ) where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy.Char8 as B (empty, readFile)
import qualified Data.ByteString.Lazy as B (ByteString)
import Data.Digest.Pure.MD5 (md5)
import Data.List (isPrefixOf)
import Data.Time (NominalDiffTime)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.Repo as R
import Debian.Repo.Monads.Top (MonadTop, sub)
import Debian.URI
import Magic
import System.FilePath (splitFileName, (</>))
import System.Directory
import System.Process (CmdSpec(..))
import System.Process.Progress (Output, timeTask, runProcessF)
import System.Unix.Directory

documentation = [ "uri:<string>:<md5sum> - A target of this form retrieves the file at the"
                , "given URI, which is assumed to be a gzipped tarball.  The optional md5sum"
                , "suffix causes the build to fail if the downloaded file does not match"
                , "this checksum.  This prevents builds when the remote tarball has changed." ]

-- | A URI that returns a tarball, with an optional md5sum which must
-- match if given.  The purpose of the md5sum is to be able to block
-- changes to the tarball on the remote host.
prepare :: R.MonadDeb e m => P.CacheRec -> P.Packages -> String -> String -> m T.Download
prepare c package u s =
    do (uri, sum, tree) <- checkTarget >>= downloadTarget >> validateTarget >>= unpackTarget
       tar <- tarball (uriToString' uri) sum
       return $ T.Download { T.package = package
                           , T.getTop = R.topdir tree
                           , T.logText = "Built from URI download " ++ (uriToString' uri)
                           , T.mVersion = Nothing
                           , T.origTarball = Just tar
                           , T.cleanTarget = \ _ -> return ([], 0)
                           , T.buildWrapper = id }
    where
      checkTarget :: (MonadTop m, MonadIO m) => m Bool
      checkTarget =
          do tar <- tarball u s
             exists <- liftIO $ doesFileExist tar
             case exists of
               True -> 
                   do (realSum :: Either SomeException String) <- liftIO $ try (B.readFile tar >>= return . show . md5)
                      case realSum of
                        Right realSum | realSum == s -> return True
                        _ -> liftIO (removeRecursiveSafely tar) >> return False
               False -> return False

      -- See if the file is already available in the checksum directory
      -- Download the target into the tmp directory, compute its checksum, and see if it matches.
      downloadTarget :: (MonadTop m, MonadIO m) => Bool -> m ()
      downloadTarget True = return ()
      downloadTarget False =
          do sum <- sumDir s
             tar <- tarball u s
             when (P.flushSource (P.params c)) (liftIO $ removeRecursiveSafely sum)
             liftIO $ createDirectoryIfMissing True sum
             exists <- liftIO $ doesFileExist tar
             _output <-
                 case exists of
                   True -> return []
                   False -> runProcessF id (ShellCommand ("curl -s '" ++ uriToString' (mustParseURI u) ++ "' > '" ++ tar ++ "'")) B.empty
             -- We should do something with the output
             return ()
      -- Make sure what we just downloaded has the correct checksum
      validateTarget :: (MonadIO m, MonadTop m) => m String
      validateTarget =
          tarball u s >>= \ tar ->
          liftIO (try (B.readFile tar >>= return . show . md5)) >>= \ (realSum :: Either SomeException String) ->
          case realSum of
            Right realSum | realSum == s -> return realSum
            Right realSum -> error ("Checksum mismatch for " ++ tar ++ ": expected " ++ s ++ ", saw " ++ realSum ++ ".")
            Left msg -> error ("Checksum failure for " ++ tar ++ ": " ++ show msg)
      unpackTarget :: (MonadIO m, MonadTop m) => String -> m (URI, FilePath, R.SourceTree)
      unpackTarget realSum =
          rmdir >> mkdir >> untar >>= read >>= search >>= verify
          where
            rmdir = sourceDir s >>= \ dir ->
                    liftIO (try (removeDirectoryRecursive dir) >>= either (\ (_ :: SomeException) -> return ()) return)
            -- Create the unpack directory
            mkdir = sourceDir s >>= \ dir ->
                    liftIO (try (createDirectoryIfMissing True dir) >>= either (\ (e :: SomeException) -> error ("Could not create " ++ dir ++ ": " ++ show e)) return)
            untar :: (MonadIO m, MonadTop m) => m ([Output B.ByteString], NominalDiffTime)
            untar =
                sourceDir s >>= \ dir ->
                tarball u s >>= \ tar -> liftIO $ 
                do magic <- magicOpen []
                   liftIO $ magicLoadDefault magic
                   fileInfo <- magicFile magic tar
                   case () of
                     _ | isPrefixOf "Zip archive data" fileInfo ->
                           timeTask $ runProcessF id (ShellCommand ("unzip " ++ tar ++ " -d " ++ dir)) B.empty
                       | isPrefixOf "gzip" fileInfo ->
                           timeTask $ runProcessF id (ShellCommand ("tar xfz " ++ tar ++ " -C " ++ dir)) B.empty
                       | isPrefixOf "bzip2" fileInfo ->
                           timeTask $ runProcessF id (ShellCommand ("tar xfj " ++ tar ++ " -C " ++ dir)) B.empty
                       | True ->
                           timeTask $ runProcessF id (ShellCommand ("cp " ++ tar ++ " " ++ dir ++ "/")) B.empty
            read (_output, _elapsed) = sourceDir s >>= liftIO . getDir
            getDir dir = getDirectoryContents dir >>= return . filter (not . flip elem [".", ".."])
            search files = checkContents (filter (not . flip elem [".", ".."]) files)
            checkContents :: (MonadIO m, MonadTop m) => [FilePath] -> m R.SourceTree
            checkContents [] = error ("Empty tarball? " ++ show (mustParseURI u))
            checkContents [subdir] =
                sourceDir s >>= \ dir ->
                liftIO (try (R.findSourceTree (dir </> subdir)) >>= either (\ (_ :: SomeException) -> R.findSourceTree dir) return)
            checkContents _ = sourceDir s >>= liftIO . R.findSourceTree
            verify tree = return (mustParseURI u, realSum, tree)

sumDir s = sub ("tmp" </> s)

tname u = snd . splitFileName . uriPath $ (mustParseURI u)

tarball u s = sub ("tmp" </> s </> tname u)
sourceDir s = sub ("tmp" </> s </> "unpack")

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Uri - parse failure: " ++ show s)) id (parseURI s)
