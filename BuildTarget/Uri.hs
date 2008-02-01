-- |A 'uri:' target is an URI that returns a tarball, with an optional
-- md5sum if we want to ensure against the tarball changing unexpectedly.
module BuildTarget.Uri where

import BuildTarget
import Debian.Types
import Debian.Types.SourceTree
import Network.URI (URI, parseURI, uriPath)
import Control.Monad
import Control.Exception
import Linspire.Unix.Directory
import Linspire.Unix.FilePath
import Data.Maybe
import System.Directory
import System.Time
import Text.Regex
import Debian.IO
import Debian.Shell
import Extra.Misc

-- | A URI that returns a tarball, with an optional md5sum which must
-- match if given.  The purpose of the md5sum is to be able to block
-- changes to the tarball on the remote host.
data Uri = Uri URI (Maybe String) SourceTree

instance Show Uri where
    show (Uri s c _) = "uri:" ++ show s ++ (maybe "" (":" ++) c)

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

    logText (Uri s _ _) _ = "Built from URI download " ++ show s

-- |Download the tarball using the URI in the target and unpack it.
prepareUri :: Bool -> FilePath -> Bool -> String -> AptIO (Either String Tgt)
prepareUri _debug top flush target =
    case parseTarget target of
      Right (uri, md5sum) -> downloadTarget uri >>= checkTarget md5sum >>= unpackTarget uri
      Left message -> return $ Left ("Invalid target " ++ target ++ ": " ++ message)
    where
      parseTarget target =
          case matchRegex (mkRegex uriRE) target of
            Just [s, _, md5sum] ->
                case parseURI s of
                  Nothing -> Left ("Invalid uri: " ++ s)
                  Just uri -> Right (uri, stringToMaybe md5sum)
            _ -> error ("Internal error 11 parsing " ++ target)
      downloadTarget :: URI -> AptIO (Either String String)
      downloadTarget uri =
          do let name = snd . splitFileName . uriPath $ uri
                 dest = tmp ++ "/" ++ name
             when flush (io . removeRecursiveSafely $ dest)
             exists <- io $ doesFileExist dest
             case exists of
               True -> return (Right name)
               False ->
                   io (try (createDirectoryIfMissing True tmp)) >>=
                   either (return . Left . show) (const (createStyle name $ runCommandQuietlyTimed ("curl -s '" ++ show uri ++ "' > '" ++ dest ++ "'"))) >>=
                   either (return . Left) (const . return . Right $ name)
      checkTarget sum (Left message) = return (Left message)
      checkTarget sum (Right name) =
          do output <- io $ md5sum path
             case output of
               Left e -> error ("Could not checksum destination file " ++ path ++ ": " ++ show e)
               Right realSum | maybe True (== realSum) sum ->
                                 do let sumDir = tmp ++ "/" ++ realSum
                                        dest = sumDir ++ "/" ++ name 
                                    io $ createDirectoryIfMissing True sumDir
                                    io $ renameFile path dest
                                    return (Right (realSum, sumDir, name))
               Right realSum -> error ("Checksum mismatch for " ++ path ++
                                       ": expected " ++ fromJust sum ++ ", saw " ++ realSum)
          where
            path = tmp ++ "/" ++ name
      unpackTarget uri (Left message) = return (Left message)
      unpackTarget uri (Right (sum, sumDir, name)) =
          do (r1 :: Either Exception ())  <- io (try (createDirectoryIfMissing True sourceDir))
             (r2 :: Either String TimeDiff) <- either (return . Left . show) (const (unpackStyle name $ runCommandQuietlyTimed ("tar xfz " ++ tarball ++ " -C " ++ sourceDir))) r1
             r3 <- either (return . Left) (const (io (getDir sourceDir))) r2
             r4 <- either (return . Left) (return . Right . filter (not . flip elem [".", ".."])) r3
             (r5 :: (Either String SourceTree)) <- either (return . Left) checkContents r4
             r6 <- return . checkSourceTree $ r5
             return r6
{-
          case contents of
            [] -> 
          do io $ createDirectoryIfMissing True sourceDir
             unpackStyle name $ runQuietlyTimed ("tar xfz " ++ tarball ++ " -C " ++ sourceDir)
             contents <- io (getDirectoryContents sourceDir) >>= return . filter (not . flip elem [".", ".."])
             sourceTree <-
                 case contents of
                   [] -> error "Empty tarball?"
                   [subdir] -> findSourceTree (rootEnvPath (sourceDir ++ "/" ++ subdir))
                   _ -> findSourceTree (rootEnvPath sourceDir)
             case sourceTree of
               Nothing -> error ("Tarball does not contain a valid debian source tree: " ++ sumDir)
               Just p -> return $ Tgt $ Uri uri (Just sum) p
-}
	  where
            getDir dir = try (getDirectoryContents dir) >>=
                         either (return . Left . show) (return . Right . filter (not . flip elem [".", ".."]))
            checkContents :: [FilePath] -> AptIO (Either String SourceTree)
            checkContents [] = return (Left "Empty tarball?")
            checkContents [subdir] = findSourceTree (rootEnvPath (sourceDir ++ "/" ++ subdir))
            checkContents _ = findSourceTree (rootEnvPath sourceDir)
            checkSourceTree :: Either String SourceTree -> Either String Tgt
            checkSourceTree (Left message) =
                Left ("Tarball in " ++ sumDir ++ " does not contain a valid debian source tree: " ++ message)
            checkSourceTree (Right p) = Right . Tgt $ Uri uri (Just sum) p
            tarball = sumDir ++ "/" ++ name
            sourceDir = sumDir ++ "/unpack"

      tmp = top ++ "/tmp"
      uriRE = "([^:]+:[^:]+)" ++ "(:(" ++ md5sumRE ++ "))?"
      md5sumRE = concat $ replicate 32 "[0-9a-fA-F]"
      stringToMaybe "" = Nothing
      stringToMaybe s = Just s
      createStyle name = setStyle (setStart (Just ("Retrieving URI for " ++ name)) .
                                   setError (Just "Curl failed") .
                                   setEcho True)
      unpackStyle name = setStyle (setStart (Just ("Unpacking " ++ name)) .
                                   setError (Just ("Failure unpacking " ++ name)) .
                                   setEcho True)
{-        
      let (uri, md5sum) =
              case ms of
                Just [uri, _, ""] -> (mustParseURI uri, Nothing)
                Just [uri, _, md5sum] -> (mustParseURI uri, Just md5sum)
                _ -> error ("failed parsing URI target: " ++ target)
      -- If the md5sum is not given we accept whatever tarball is retrieved
      -- from the URL.  If it is given we check whether it is already downloaded
      -- and if not, we download it and verify its md5sum.
      (finalPath, checksumDir) <-
          case md5sum of
            Nothing ->
                do
                  -- We assume that whatever file is already
                  -- downloaded is correct and up-to-date.  This is
                  -- not perfectly safe, but the uri target without a
                  -- checksum is inherantly unsafe (and packages built
                  -- from these targets are not allowed to be uploaded
                  -- to a remote repository.)
                  let downloadPath = download +/+ name uri
                  realSum <- maybeDownload flush uri downloadPath
                  let checksumDir = download +/+ realSum
                  let finalPath = checksumDir +/+ name uri
                  io $ createDirectoryIfMissing True checksumDir
                  -- Link the file into the checksum Directory.
                  io $ createLink downloadPath finalPath
                  return (finalPath, checksumDir)
            Just sum ->
                do
                  let checksumDir = download +/+ sum
                  let finalPath = checksumDir +/+ name uri
                  realSum <- maybeDownload flush uri finalPath
                  if sum /= realSum then
                      error ("MD5sum mismatch on " ++ finalPath ++ ": expected " ++ sum ++ ", actual " ++ realSum) else
                      return (finalPath, checksumDir)
      -- Make sure the tarball has a top directory and unpack
      tardir <- io $ tarDir finalPath
      unpackStyle uri $ systemTask ("tar xfz " ++ finalPath ++ " -C " ++ checksumDir)
      io $ createDirectoryIfMissing True checksumDir
      return $ Tgt $ Uri uri md5sum (DebianBuildTree checksumDir tardir)
    where
      ms = match uriRE target
      match = matchRegex . mkRegex
      uriRE = "([^:]+:[^:]+)" ++ "(:(" ++ md5sumRE ++ "))?"
      md5sumRE = concat $ replicate 32 "[0-9a-fA-F]"
      maybeDownload :: Bool -> URI -> FilePath -> AptIO FilePath
      maybeDownload flush uri downloadPath =
          do
            when flush (io $ removeRecursiveSafely downloadPath)
            exists <- io $ doesFileExist downloadPath
            if exists then
                return (Right noTimeDiff) else
                do
                  io $ createDirectoryIfMissing True download
                  createStyle uri $ systemTask_ ("curl -s -g " ++ show uri ++ " > '" ++ downloadPath ++ "'")
            (io $ My.md5sum downloadPath) >>= return. either error id
      createStyle uri = setStyle (setStart (Just ("Retrieving URI for " ++ show uri)) .
                                  setError (Just "Curl failed") .
                                  setEcho True)
      unpackStyle uri = setStyle (setStart (Just ("Unpacking " ++ name uri)) .
                                  setError (Just ("Failure unpacking " ++ name uri)) .
                                  setEcho True)
      download = top ++ "/download"
      name uri = snd . splitFileName $ show uri
-}

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)
