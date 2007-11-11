-- |A 'uri:' target is an URI that returns a tarball, with an optional
-- md5sum if we want to ensure against the tarball changing unexpectedly.
module BuildTarget.Uri where

import BuildTarget
import Debian.SourceTree
import Debian.Types
import Network.URI (URI, parseURI, uriPath)
import Control.Monad
import Linspire.Unix.Directory
import Linspire.Unix.FilePath
import Data.Maybe
import System.Directory
import System.Time
import Text.Regex
import Debian.IO
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
    getTop (Uri _ _ tree) = dir tree
    -- The revision string for a URI target is the md5sum if it is known.
    -- If it isn't known, we raise an error to avoid mysterious things
    -- happening with URI's that, for example, always point to the latest
    -- version of a package.
    revision (Uri _ (Just c) _) = return (Just c)
    revision (Uri _ Nothing _) = return Nothing

    logText (Uri s _ _) _ = "Built from URI download " ++ show s

-- |Download the tarball using the URI in the target and unpack it.
prepareUri :: Bool -> FilePath -> Bool -> String -> AptIO Tgt
prepareUri _debug top flush target =
    case parseTarget target of
      Just (uri, md5sum) -> downloadTarget uri >>= checkTarget md5sum >>= unpackTarget uri
      Nothing -> error ("Invalid target: " ++ target)
    where
      parseTarget target =
          case matchRegex (mkRegex uriRE) target of
            Just [s, _, md5sum] ->
                case parseURI s of
                  Nothing -> error ("Invalid uri: " ++ s)
                  Just uri -> Just (uri, stringToMaybe md5sum)
            _ -> error ("Internal error 11 parsing " ++ target)
      downloadTarget uri =
          do let name = snd . splitFileName . uriPath $ uri
                 dest = tmp ++ "/" ++ name
             when flush (io . removeRecursiveSafely $ dest)
             exists <- io $ doesFileExist dest
             if exists then
                 return noTimeDiff else
                 do io $ createDirectoryIfMissing True tmp
                    createStyle name $ systemTask_ ("curl -s '" ++ show uri ++ "' > '" ++ dest ++ "'")
             return name
      checkTarget sum name =
          do output <- io $ md5sum path
             case output of
               Left e -> error ("Could not checksum destination file " ++ path ++ ": " ++ show e)
               Right realSum | maybe True (== realSum) sum ->
                                 do let sumDir = tmp ++ "/" ++ realSum
                                        dest = sumDir ++ "/" ++ name 
                                    io $ createDirectoryIfMissing True sumDir
                                    io $ renameFile path dest
                                    return (realSum, sumDir, name)
               Right realSum -> error ("Checksum mismatch for " ++ path ++
                                       ": expected " ++ fromJust sum ++ ", saw " ++ realSum)
          where
            path = tmp ++ "/" ++ name
      unpackTarget uri (sum, sumDir, name) =
          do let tarball = sumDir ++ "/" ++ name
             tardir <- io $ tarDir tarball
             let sourceDir = sumDir ++ "/" ++ maybe "source" id tardir
             io $ createDirectoryIfMissing True sourceDir
             unpackStyle name $ systemTask ("tar xfz " ++ tarball ++ " -C " ++ sourceDir)
             source <- findSourceTree (rootEnvPath sourceDir)
             case source of
               Nothing -> error ("Tarball does not contain a valid debian source tree: " ++ sumDir)
               Just p -> return $ Tgt $ Uri uri (Just sum) p
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
