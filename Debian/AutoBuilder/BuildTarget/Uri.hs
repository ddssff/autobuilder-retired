-- |A 'uri:' target is an URI that returns a tarball, with an optional
-- md5sum if we want to ensure against the tarball changing unexpectedly.
module Debian.AutoBuilder.BuildTarget.Uri where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.List (isPrefixOf)
import Data.Maybe
import Debian.AutoBuilder.BuildTarget
import Debian.Repo
import Debian.Shell
import Debian.URI
import Extra.CIO
import Extra.Misc
import Magic
import System.FilePath (splitFileName)
import System.Unix.Directory
import System.Directory
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
    getTop (Uri _ _ tree) = topdir tree
    -- The revision string for a URI target is the md5sum if it is known.
    -- If it isn't known, we raise an error to avoid mysterious things
    -- happening with URI's that, for example, always point to the latest
    -- version of a package.
    revision (Uri _ (Just c) _) = return (Right c)
    revision (Uri _ Nothing _) = return (Left "Uri targets with no checksum do not have revision strings")

    logText (Uri s _ _) _ = "Built from URI download " ++ uriToString' s

-- |Download the tarball using the URI in the target and unpack it.
prepareUri :: CIO m => Bool -> FilePath -> Bool -> String -> m (Either String Tgt)
prepareUri _debug top flush target =
    case parseTarget target of
      Right (uri, md5sum) -> checkTarget uri md5sum >>= downloadTarget uri md5sum >>= validateTarget md5sum >>= unpackTarget uri
      Left message -> return $ Left ("Invalid target " ++ target ++ ": " ++ message)
    where
      parseTarget target =
          case matchRegex (mkRegex uriRE) target of
            Just [s, md5sum] ->
                case parseURI s of
                  Nothing -> Left ("Invalid uri: " ++ s)
                  Just uri -> Right (uri, md5sum)
            _ -> error ("Syntax error in URI target, expected uri:<tarballuri>:<md5sum>, found " ++ target)
      checkTarget uri sum =
          liftIO $ doesFileExist final
          where
            final = tmp ++ "/" ++ sum ++ "/" ++ name
            name = snd . splitFileName . uriPath $ uri
      -- See if the file is already available in the checksum directory
      -- Download the target into the tmp directory, compute its checksum, and see if it matches.
      downloadTarget :: CIO m => URI -> String -> Bool -> m (Either String String)
      downloadTarget uri sum True =
          return (Right name)
          where
            name = snd . splitFileName . uriPath $ uri
      downloadTarget uri sum False =
          do when flush (liftIO . removeRecursiveSafely $ sumDir)
             liftIO $ createDirectoryIfMissing True sumDir
             exists <- liftIO $ doesFileExist dest
             case exists of
               True -> return (Right name)
               False ->
                   runCommand 1 ("curl -s '" ++ uriToString' uri ++ "' > '" ++ dest ++ "'") >>=
                   either (return . Left) (const . return . Right $ name)
          where
            dest = sumDir ++ "/" ++ name
            sumDir = tmp ++ "/" ++ sum
            name = snd . splitFileName . uriPath $ uri
      -- Make sure what we just downloaded has the correct checksum
      validateTarget :: CIO m => String -> Either String String -> m (Either String (String, String, String))
      validateTarget sum (Left x) = return (Left x)
      validateTarget sum (Right name) =
          do output <- liftIO $ md5sum dest
             case output of
               Left e -> return (Left ("Could not checksum destination file " ++ dest ++ ": " ++ show e))
               -- We have checksummed the file and it either matches
               -- what we expected or we don't know what checksum to
               -- expect.
               Right realSum
                   | sum == realSum ->
                       return (Right (realSum, sumDir, name))
                   | True ->
                       -- We have checksummed the file but it doesn't match
                       do return (Left ("Checksum mismatch for " ++ dest ++
                                        ": expected " ++ sum ++ ", saw " ++ realSum ++ "."))
          where
            dest = sumDir ++ "/" ++ name
            sumDir = tmp ++ "/" ++ sum
      unpackTarget :: CIO m => URI -> Either String (String, String, String) -> m (Either String Tgt)
      unpackTarget _ (Left message) = return (Left message)
      unpackTarget uri (Right (sum, sumDir, name)) =
          mkdir >>= untar >>= read >>= search >>= verify
          where
            -- Create the unpack directory
            mkdir = liftIO (try (createDirectoryIfMissing True sourceDir))
            untar (Left e) = return . Left . show $ e
            untar (Right ()) =
                do c <- liftIO (unpackChar tarball)
                   runCommandTimed 1 ("tar xf" ++ c ++ " " ++ tarball ++ " -C " ++ sourceDir)
            unpackChar tarball =
                do magic <- magicOpen []
                   magicLoadDefault magic
                   fileInfo <- magicFile magic tarball
                   return $ if isPrefixOf "gzip" fileInfo 
                            then "z"
                            else if isPrefixOf "bzip2" fileInfo
                                 then "j"
                                 else ""
            read (Left message) = return . Left $ message
            read (Right (output, elapsed)) = liftIO (getDir sourceDir)
            search (Left message) = return . Left $ message
            search (Right files) = checkContents (filter (not . flip elem [".", ".."]) files)
            verify (Left message) = return . Left $ ("Tarball in " ++ sumDir ++ " does not contain a valid debian source tree: " ++ message)
            verify (Right tree) = return . Right . Tgt $ Uri uri (Just sum) tree
            getDir dir = try (getDirectoryContents dir) >>=
                         either (return . Left . show) (return . Right . filter (not . flip elem [".", ".."]))
            checkContents :: CIO m => [FilePath] -> m (Either String SourceTree)
            checkContents [] = return (Left "Empty tarball?")
            checkContents [subdir] = findSourceTree (rootEnvPath (sourceDir ++ "/" ++ subdir))
            checkContents _ = findSourceTree (rootEnvPath sourceDir)
            tarball = sumDir ++ "/" ++ name
            sourceDir = sumDir ++ "/unpack"

      tmp = top ++ "/tmp"
      uriRE = "([^:]+:[^:]+):(" ++ md5sumRE ++ ")"
      md5sumRE = concat $ replicate 32 "[0-9a-fA-F]"
      stringToMaybe "" = Nothing
      stringToMaybe s = Just s

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)
