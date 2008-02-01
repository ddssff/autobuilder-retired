module BuildTarget.Darcs where

import BuildTarget
import Debian.Types
import Debian.Types.SourceTree
import System.Directory
import System.Process hiding (runCommand)
import System.IO
import Linspire.Unix.Process
import Text.Regex
import Control.Exception
import Control.Monad
import Linspire.Unix.Directory
import Linspire.Unix.FilePath
import Debian.IO
import Debian.Shell
import DryRun

-- | A Darcs archive
data Darcs = Darcs { uri :: String
                   , tag :: Maybe String
                   , sourceTree :: SourceTree }

documentation = [ "darcs:<string> - a target of this form obtains the source code by running"
                , "darcs get <string>.  If the argument needs to use ssh to reach the darcs"
                , "repository, it is necessary to set up ssh keys to allow access without"
                , "typing a password.  See the --ssh-export option for help doing this." ]

instance Show Darcs where
    show t = "darcs:" ++ uri t

instance BuildTarget Darcs where
    getTop t = topdir (sourceTree t)
    cleanTarget _ path = 
        cleanStyle path $ runCommandQuietlyTimed cmd
        where 
          cmd = "find " ++ outsidePath path ++ " -name '_darcs' -maxdepth 1 -prune | xargs rm -rf"
          cleanStyle path = setStyle $ setStart (Just (" Copy and clean TLA target to " ++ show path))
    revision tgt =
        do (_, outh, _, handle) <- io $ runInteractiveCommand cmd
           revision <- io (hGetContents outh >>= return . matchRegex (mkRegex "hash='([^']*)'") >>=
                           return . maybe (Left $ "could not find hash field in output of '" ++ cmd ++ "'") (Right . head))
           case revision of
             Left message -> return (Left message)
             Right revision ->
                 do io $ evaluate (length revision)
                    io $ waitForProcess handle
                    return . Right $ show tgt ++ "=" ++ revision
        where
          path = topdir (sourceTree tgt)
          cmd = "cd " ++ outsidePath path ++ " && darcs changes --xml-output"
    logText _ revision = "Darcs revision: " ++ maybe "none" id revision

prepareDarcs :: Bool -> FilePath -> Bool -> String -> AptIO (Either String Tgt)
prepareDarcs _debug top flush uriAndTag =
    do
      when flush (removeRecursiveSafelyDR dir)
      exists <- io $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      case tree of 
        Left message -> return (Left message)
        Right tree -> return . Right . Tgt $ Darcs { uri = theUri, tag = theTag, sourceTree = tree }
    where
      verifySource :: FilePath -> AptIO (Either String SourceTree)
      verifySource dir =
          -- Note that this logic is the opposite of 'tla changes'
          do result <- verifyStyle $ io (lazyCommand ("cd " ++ dir ++ " && darcs whatsnew") [] >>= return . discardOutput)
             case result of
               [Result (ExitFailure n)] -> updateSource dir				-- No Changes!
               [Result ExitSuccess] -> removeSource dir >> createSource dir		-- Yes changes
               _ -> error "Internal error"
      removeSource :: FilePath -> AptIO ()
      removeSource dir = io $ removeRecursiveSafely dir

      updateSource :: FilePath -> AptIO (Either String SourceTree)
      updateSource dir =
          updateStyle (runCommandQuietly ("cd " ++ dir ++ " && darcs pull --all " ++ theUri)) >>=
          either (return . Left) (const (findSourceTree (rootEnvPath dir))) >>=
          return . either (\ message -> Left $ "Couldn't find sourceTree at " ++ dir ++ ": " ++ message) Right

      createSource :: FilePath -> AptIO (Either String SourceTree)
      createSource dir =
          let (parent, _) = splitFileName dir in
          do r1 <- io (try (createDirectoryIfMissing True parent))
             r2 <- either (return . Left . show) (const (createStyle $ runCommandQuietly cmd)) r1
             r3 <- either (return . Left) (const (findSourceTree (rootEnvPath dir))) r2
             let r4 = either (\ message -> Left $ "Couldn't find sourceTree at " ++ dir ++ ": " ++ message) Right r3
             return r4
          where
            cmd = unwords $ ["darcs", "get", "--partial", theUri] ++ maybe [] (\ tag -> [" --tag", "'" ++ tag ++ "'"]) theTag ++ [dir]
{-
          do
            -- Create parent dir and let tla create dir
            let (parent, _) = splitFileName dir
            io $ createDirectoryIfMissing True parent
            createStyle . systemTask . unwords $ ["darcs", "get", "--partial", theUri] ++ maybe [] (\ tag -> [" --tag", "'" ++ tag ++ "'"]) theTag ++ [dir]
            findSourceTree (rootEnvPath dir) >>= return . maybe (error ("Couldn't find sourceTree at " ++ dir)) id
-}
      verifyStyle = setStyle (setStart (Just ("Verifying Darcs source archive " ++ theUri)) .
                              setError Nothing .
                              quietStyle stderr .
                              setEcho True)
      updateStyle = setStyle (setStart (Just ("Updating Darcs source for " ++ theUri)) . setEcho True .
                              addPrefixes " " " " . setError (Just "updateSource failed"))
      createStyle = setStyle (setStart (Just ("Retrieving Darcs source for " ++  theUri)) . setEcho True .
                              addPrefixes " " " " . setError (Just ("darcs get failed in " ++ dir)))
      name = snd . splitFileName $ theUri
      (theUri, theTag) =
          case matchRegex (mkRegex "^(.*)(=([^=]*))?$") uriAndTag of
            Just [uri, "", _] -> (uri, Nothing)
            Just [uri, _, tag] -> (uri, Just tag)
            _ -> error "Internal error"	-- That regex should always match
      dir = top ++ "/darcs/" ++ name
