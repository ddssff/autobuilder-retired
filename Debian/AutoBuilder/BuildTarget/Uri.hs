{-# LANGUAGE ScopedTypeVariables #-}
-- |A 'uri:' target is an URI that returns a tarball, with an optional
-- md5sum if we want to ensure against the tarball changing unexpectedly.
module Debian.AutoBuilder.BuildTarget.Uri where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy.Char8 (empty)
import Data.List (isPrefixOf)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Params as P
import Debian.Repo
--import Debian.OldShell (runCommand, runCommandTimed)
import Debian.URI
import Extra.Misc as Extra
import Magic
import System.FilePath (splitFileName)
import System.Directory
import System.Unix.Directory
import System.Unix.Progress (lazyCommandF, timeTask)
import Text.Regex

-- | A URI that returns a tarball, with an optional md5sum which must
-- match if given.  The purpose of the md5sum is to be able to block
-- changes to the tarball on the remote host.
data Uri = Uri URI (Maybe String) SourceTree

instance Show Uri where
    show (Uri s c _) = "uri:" ++ uriToString' s ++ (maybe "" (":" ++) c)

documentation = [ "uri:<string>:<md5sum> - A target of this form retrieves the file at the"
                , "given URI, which is assumed to be a gzipped tarball.  The optional md5sum"
                , "suffix causes the build to fail if the downloaded file does not match"
                , "this checksum.  This prevents builds when the remote tarball has changed." ]

instance BuildTarget Uri where
    getTop _ (Uri _ _ tree) = topdir tree
    -- The revision string for a URI target is the md5sum if it is known.
    -- If it isn't known, we raise an error to avoid mysterious things
    -- happening with URI's that, for example, always point to the latest
    -- version of a package.
    revision _ (Uri _ (Just c) _) = return c
    revision _ (Uri _ Nothing _) = fail "Uri targets with no checksum do not have revision strings"

    logText (Uri s _ _) _ = "Built from URI download " ++ uriToString' s

-- |Download the tarball using the URI in the target and unpack it.
prepare :: P.CacheRec -> String -> AptIOT IO Uri
prepare cache target =
    prepare' cache uri md5sum
    where
      (uri, md5sum) = parseTarget target
      parseTarget target =
          case matchRegex (mkRegex uriRE) target of
            Just [s, md5sum] -> (s, md5sum)
            _ -> error ("Syntax error in URI target, expected uri:<tarballuri>:<md5sum>, found " ++ target)
      uriRE = "([^:]+:[^:]+):(" ++ md5sumRE ++ ")"
      md5sumRE = concat $ replicate 32 "[0-9a-fA-F]"

prepare' :: P.CacheRec -> String -> String -> AptIOT IO Uri
prepare' cache uri md5sum = liftIO $
    checkTarget uri' md5sum >>=
    downloadTarget uri' md5sum >>=
    validateTarget md5sum >>=
    unpackTarget uri'
    where
      uri' = mustParseURI uri
      checkTarget uri sum =
          doesFileExist final
          where
            final = tmp ++ "/" ++ sum ++ "/" ++ name
            name = snd . splitFileName . uriPath $ uri
      -- See if the file is already available in the checksum directory
      -- Download the target into the tmp directory, compute its checksum, and see if it matches.
      downloadTarget :: URI -> String -> Bool -> IO String
      downloadTarget uri _sum True =
          return name
          where name = snd . splitFileName . uriPath $ uri
      downloadTarget uri sum False =
          do when (P.flushSource (P.params cache)) (removeRecursiveSafely sumDir)
             createDirectoryIfMissing True sumDir
             exists <- doesFileExist dest
             case exists of
               True -> return name
               False -> 
                   -- runCommand 1 ("curl -s '" ++ uriToString' uri ++ "' > '" ++ dest ++ "'") >>
                   lazyCommandF ("curl -s '" ++ uriToString' uri ++ "' > '" ++ dest ++ "'") empty >>
                   return name
          where
            dest = sumDir ++ "/" ++ name
            sumDir = tmp ++ "/" ++ sum
            name = snd . splitFileName . uriPath $ uri
      -- Make sure what we just downloaded has the correct checksum
      validateTarget :: String -> String -> IO (String, String, String)
      validateTarget sum name =
          liftIO (Extra.md5sum dest) >>= \ realSum ->
          case realSum of
            Right realSum' | sum == realSum' -> return (realSum', sumDir, name)
            Right realSum' -> error ("Checksum mismatch for " ++ dest ++
                                     ": expected " ++ sum ++ ", saw " ++ realSum' ++ ".")
            Left msg -> error ("Checksum failure for " ++ dest ++ ": " ++ msg)
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
          where
            dest = sumDir ++ "/" ++ name
            sumDir = tmp ++ "/" ++ sum
      unpackTarget :: URI -> (String, String, String) -> IO Uri
      unpackTarget uri (sum, sumDir, name) =
          mkdir >> untar >>= read >>= search >>= verify
          where
            -- Create the unpack directory
            mkdir = liftIO (createDirectoryIfMissing True sourceDir)
            untar =
                do c <- liftIO (unpackChar tarball)
                   timeTask (lazyCommandF ("tar xf" ++ c ++ " " ++ tarball ++ " -C " ++ sourceDir) empty)
                   -- runCommandTimed 1 ("tar xf" ++ c ++ " " ++ tarball ++ " -C " ++ sourceDir)
            unpackChar tarball =
                do magic <- magicOpen []
                   magicLoadDefault magic
                   fileInfo <- magicFile magic tarball
                   return $ if isPrefixOf "gzip" fileInfo 
                            then "z"
                            else if isPrefixOf "bzip2" fileInfo
                                 then "j"
                                 else ""
            read (_output, _elapsed) = liftIO (getDir sourceDir)
            search files = checkContents (filter (not . flip elem [".", ".."]) files)
            verify tree = return $ Uri uri (Just sum) tree
            getDir dir = getDirectoryContents dir >>= return . filter (not . flip elem [".", ".."])
            checkContents :: [FilePath] -> IO SourceTree
            checkContents [] = error ("Empty tarball? " ++ show uri)
            checkContents [subdir] = findSourceTree (sourceDir ++ "/" ++ subdir)
            checkContents _ = findSourceTree sourceDir
            tarball = sumDir ++ "/" ++ name
            sourceDir = sumDir ++ "/unpack"

      tmp = P.topDir cache ++ "/tmp"
