-- |A 'uri:' target is an URI that returns a tarball, with an optional
-- md5sum if we want to ensure against the tarball changing unexpectedly.
module BuildTarget.Uri where

import Control.Monad.Trans
import BuildTarget
import Debian.Types
import Debian.Types.SourceTree
import Ugly.URI
import Control.Monad
import Control.Exception
import System.Unix.Directory
import System.Unix.FilePath
--import System.Unix.Process
import Data.Maybe
import System.Directory
--import System.Time
import Text.Regex
import Debian.TIO
import Debian.Shell
import Extra.Misc

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
prepareUri :: Bool -> FilePath -> Bool -> String -> TIO (Either String Tgt)
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
      downloadTarget :: URI -> TIO (Either String String)
      downloadTarget uri =
          do let name = snd . splitFileName . uriPath $ uri
                 dest = tmp ++ "/" ++ name
             when flush (lift . removeRecursiveSafely $ dest)
             exists <- lift $ doesFileExist dest
             case exists of
               True -> return (Right name)
               False ->
                   lift (try (createDirectoryIfMissing True tmp)) >>=
                   either (return . Left . show) (const ({- createStyle name $ -} runCommand 1 ("curl -s '" ++ uriToString' uri ++ "' > '" ++ dest ++ "'"))) >>=
                   either (return . Left) (const . return . Right $ name)
      checkTarget _ (Left message) = return (Left message)
      checkTarget sum (Right name) =
          do output <- lift $ md5sum path
             case output of
               Left e -> error ("Could not checksum destination file " ++ path ++ ": " ++ show e)
               Right realSum | maybe True (== realSum) sum ->
                                 do let sumDir = tmp ++ "/" ++ realSum
                                        dest = sumDir ++ "/" ++ name 
                                    lift $ createDirectoryIfMissing True sumDir
                                    lift $ renameFile path dest
                                    return (Right (realSum, sumDir, name))
               Right realSum -> error ("Checksum mismatch for " ++ path ++
                                       ": expected " ++ fromJust sum ++ ", saw " ++ realSum)
          where
            path = tmp ++ "/" ++ name
      unpackTarget _ (Left message) = return (Left message)
      unpackTarget uri (Right (sum, sumDir, name)) =
          mkdir >>= untar >>= read >>= search >>= verify
          where
            mkdir = lift (try (createDirectoryIfMissing True sourceDir))
            untar (Left e) = return . Left . show $ e
            untar (Right ()) = {- unpackStyle name $ -} runCommandTimed 1 ("tar xfz " ++ tarball ++ " -C " ++ sourceDir)
            read (Left message) = return . Left $ message
            read (Right (output, elapsed)) = lift (getDir sourceDir)
            search (Left message) = return . Left $ message
            search (Right files) = checkContents (filter (not . flip elem [".", ".."]) files)
            verify (Left message) = return . Left $ ("Tarball in " ++ sumDir ++ " does not contain a valid debian source tree: " ++ message)
            verify (Right tree) = return . Right . Tgt $ Uri uri (Just sum) tree
{-
          do (r1 :: Either Exception ())  <- lift (try (createDirectoryIfMissing True sourceDir))
             (r2 :: Either String ([Output], TimeDiff)) <- either (return . Left . show) (const (unpackStyle name $ runCommandTimed 1 ("tar xfz " ++ tarball ++ " -C " ++ sourceDir))) r1
             r3 <- either (return . Left) (const (lift (getDir sourceDir))) r2
             r4 <- either (return . Left) (return . Right . filter (not . flip elem [".", ".."])) r3
             (r5 :: (Either String SourceTree)) <- either (return . Left) checkContents r4
             r6 <- return . checkSourceTree $ r5
             return r6
	  where
-}
            getDir dir = try (getDirectoryContents dir) >>=
                         either (return . Left . show) (return . Right . filter (not . flip elem [".", ".."]))
            checkContents :: [FilePath] -> TIO (Either String SourceTree)
            checkContents [] = return (Left "Empty tarball?")
            checkContents [subdir] = findSourceTree (rootEnvPath (sourceDir ++ "/" ++ subdir))
            checkContents _ = findSourceTree (rootEnvPath sourceDir)
{-
            checkSourceTree :: Either String SourceTree -> Either String Tgt
            checkSourceTree (Left message) =
                Left ("Tarball in " ++ sumDir ++ " does not contain a valid debian source tree: " ++ message)
            checkSourceTree (Right p) = Right . Tgt $ Uri uri (Just sum) p
-}
            tarball = sumDir ++ "/" ++ name
            sourceDir = sumDir ++ "/unpack"

      tmp = top ++ "/tmp"
      uriRE = "([^:]+:[^:]+)" ++ "(:(" ++ md5sumRE ++ "))?"
      md5sumRE = concat $ replicate 32 "[0-9a-fA-F]"
      stringToMaybe "" = Nothing
      stringToMaybe s = Just s
{-
      createStyle name = setStyle (setStart (Just ("Retrieving URI for " ++ name)) .
                                   setError (Just "Curl failed") .
                                   setEcho True)
      unpackStyle name = setStyle (setStart (Just ("Unpacking " ++ name)) .
                                   setError (Just ("Failure unpacking " ++ name)) .
                                   setEcho True)
-}

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)
