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
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B (empty, readFile)
import Data.Digest.Pure.MD5 (md5)
import Data.List (isPrefixOf)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.Repo as R
--import Debian.OldShell (runCommand, runCommandTimed)
import Debian.URI
import Magic
import System.FilePath (splitFileName)
import System.Directory
import System.Unix.Directory
import System.Unix.Progress (lazyCommandF, timeTask)

-- | A URI that returns a tarball, with an optional md5sum which must
-- match if given.  The purpose of the md5sum is to be able to block
-- changes to the tarball on the remote host.
data Uri = Uri URI (Maybe String) R.SourceTree

instance Show Uri where
    show (Uri s c _) = "uri:" ++ uriToString' s ++ (maybe "" (":" ++) c)

documentation = [ "uri:<string>:<md5sum> - A target of this form retrieves the file at the"
                , "given URI, which is assumed to be a gzipped tarball.  The optional md5sum"
                , "suffix causes the build to fail if the downloaded file does not match"
                , "this checksum.  This prevents builds when the remote tarball has changed." ]

instance BuildTarget Uri where
    getTop _ (Uri _ _ tree) = R.topdir tree
    -- The revision string for a URI target is the md5sum if it is known.
    -- If it isn't known, we raise an error to avoid mysterious things
    -- happening with URI's that, for example, always point to the latest
    -- version of a package.
    revision _ (Uri _ (Just c) _) = return c
    revision _ (Uri _ Nothing _) = fail "Uri targets with no checksum do not have revision strings"

    logText (Uri s _ _) _ = "Built from URI download " ++ uriToString' s
    origTarball c (Uri u (Just s) _) = Just (tarball c (uriToString' u) s)
    origTarball _ _ = Nothing

-- |Download the tarball using the URI in the target and unpack it.
prepare :: P.CacheRec -> String -> String -> R.AptIOT IO Uri
prepare c u s = liftIO $
    checkTarget >>=
    downloadTarget >>
    validateTarget >>=
    unpackTarget
    where
      checkTarget = doesFileExist (tarball c u s)

      -- See if the file is already available in the checksum directory
      -- Download the target into the tmp directory, compute its checksum, and see if it matches.
      downloadTarget :: Bool -> IO ()
      downloadTarget True = return ()
      downloadTarget False =
          do when (P.flushSource (P.params c)) (removeRecursiveSafely (sumDir c s))
             createDirectoryIfMissing True (sumDir c s)
             exists <- doesFileExist (tarball c u s)
             _output <-
                 case exists of
                   True -> return []
                   False -> lazyCommandF ("curl -s '" ++ uriToString' (uri u) ++ "' > '" ++ tarball c u s ++ "'") B.empty
             -- We should do something with the output
             return ()
      -- Make sure what we just downloaded has the correct checksum
      validateTarget :: IO String
      validateTarget =
          liftIO (try (B.readFile (tarball c u s) >>= return . show . md5)) >>= \ (realSum :: Either SomeException String) ->
          case realSum of
            Right realSum | realSum == s -> return realSum
            Right realSum -> error ("Checksum mismatch for " ++ tarball c u s ++ ": expected " ++ s ++ ", saw " ++ realSum ++ ".")
            Left msg -> error ("Checksum failure for " ++ tarball c u s ++ ": " ++ show msg)
{-
          do realSum <- liftIO $ Extra.md5sum dest
             -- We have checksummed the file and it either matches
             -- what we expected or we don't know what checksum to
             -- expect.
             if Right sum == realSum then
                 return (realSum, sumDir, name) else
             -- We have checksummed the file but it doesn't match
                 error ("Checksum mismatch for " ++ dest ++
                        ": expected " ++ sum ++ ", saw " ++ realSum ++ ".")
-}
      unpackTarget :: String -> IO Uri
      unpackTarget realSum =
          mkdir >> untar >>= read >>= search >>= verify
          where
            -- Create the unpack directory
            mkdir = liftIO (createDirectoryIfMissing True (sourceDir c s))
            untar =
                do ch <- liftIO unpackChar
                   timeTask (lazyCommandF ("tar xf" ++ ch ++ " " ++ tarball c u s ++ " -C " ++ sourceDir c s) B.empty)
                   -- runCommandTimed 1 ("tar xf" ++ c ++ " " ++ tarball c u s ++ " -C " ++ sourceDir c s)
            unpackChar =
                do magic <- magicOpen []
                   magicLoadDefault magic
                   fileInfo <- magicFile magic (tarball c u s)
                   return $ if isPrefixOf "gzip" fileInfo 
                            then "z"
                            else if isPrefixOf "bzip2" fileInfo
                                 then "j"
                                 else ""
            read (_output, _elapsed) = liftIO (getDir (sourceDir c s))
            search files = checkContents (filter (not . flip elem [".", ".."]) files)
            verify tree = return $ Uri (uri u) (Just realSum) tree
            getDir dir = getDirectoryContents dir >>= return . filter (not . flip elem [".", ".."])
            checkContents :: [FilePath] -> IO R.SourceTree
            checkContents [] = error ("Empty tarball? " ++ show (uri u))
            checkContents [subdir] = R.findSourceTree (sourceDir c s ++ "/" ++ subdir)
            checkContents _ = R.findSourceTree (sourceDir c s)

uri u = mustParseURI u

sumDir c s = P.topDir c ++ "/tmp/" ++ s

tname u = snd . splitFileName . uriPath $ (mustParseURI u)

tarball c u s = sumDir c s ++ "/" ++ tname u
sourceDir c s = sumDir c s ++ "/unpack"
