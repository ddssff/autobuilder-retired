module BuildTarget.Darcs where

import BuildTarget
import Debian.SourceTree
import Debian.Types
import System.Directory
import System.Exit
import System.Process
import System.IO
import Text.Regex
import Control.Exception
import Control.Monad
import Linspire.Unix.Directory
import Linspire.Unix.FilePath
import Debian.IO
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
    getTop t = dir (sourceTree t)
    cleanTarget _ source = 
        do let path = dir source
               cmd = "find " ++ outsidePath path ++ " -name '_darcs' -maxdepth 1 -prune | xargs rm -rf"
           cleanStyle path $ systemTask_ cmd
           return ()
        where cleanStyle path = setStyle $ setStart (Just (" Copy and clean TLA target to " ++ show path))
    revision tgt =
        do let path = dir (sourceTree tgt)
               cmd = "cd " ++ outsidePath path ++ " && darcs changes --xml-output"
           -- FIXME: this command can take a lot of time, message it
           (_, outh, _, handle) <- io $ runInteractiveCommand cmd
           revision <- io (hGetContents outh >>= return . matchRegex (mkRegex "hash='([^']*)'") >>=
                           return . maybe (error "could not find hash field in output of '" ++ cmd ++ "'") head)
           io $ evaluate (length revision)
           io $ waitForProcess handle
           return . Just $ show tgt ++ "=" ++ revision
    logText _ revision = "Darcs revision: " ++ maybe "none" id revision

prepareDarcs :: Bool -> FilePath -> Bool -> String -> AptIO Tgt
prepareDarcs _debug top flush uriAndTag =
    do
      when flush (removeRecursiveSafelyDR dir)
      exists <- io $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      return $ Tgt $ Darcs { uri = theUri, tag = theTag, sourceTree = tree }
    where
      verifySource :: FilePath -> AptIO SourceTree
      verifySource dir =
          -- Note that this logic is the opposite of 'tla changes'
          do result <- verifyStyle $ systemTask'_ ("cd " ++ dir ++ " && darcs whatsnew")
             case result of
               (ExitFailure _, _) -> updateSource dir
               (ExitSuccess, _) -> removeSource dir >> createSource dir
      removeSource :: FilePath -> AptIO ()
      removeSource dir = io $ removeRecursiveSafely dir

      updateSource :: FilePath -> AptIO SourceTree
      updateSource dir =
          do
            updateStyle $ systemTask ("cd " ++ dir ++ " && darcs pull --all " ++ theUri)
            -- At one point we did a tla undo here.  However, we are
            -- going to assume that the "clean" copies in the cache
            -- directory are clean, since some of the other target
            -- types have no way of doing this reversion.
            findSourceTree (rootEnvPath dir) >>= return . maybe (error ("Couldn't find sourceTree at " ++ dir)) id

      createSource :: FilePath -> AptIO SourceTree
      createSource dir =
          do
            -- Create parent dir and let tla create dir
            let (parent, _) = splitFileName dir
            io $ createDirectoryIfMissing True parent
            createStyle . systemTask . unwords $ ["darcs", "get", "--partial", theUri] ++ maybe [] (\ tag -> [" --tag", "'" ++ tag ++ "'"]) theTag ++ [dir]
            findSourceTree (rootEnvPath dir) >>= return . maybe (error ("Couldn't find sourceTree at " ++ dir)) id
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
