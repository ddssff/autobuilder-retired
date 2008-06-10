module Debian.AutoBuilder.BuildTarget.Darcs where

import Debian.Repo
import Debian.Shell

import Debian.AutoBuilder.BuildTarget
import Control.Monad.Trans
import System.Directory
import System.Process hiding (runCommand)
import System.IO
import System.Unix.Process
import Text.Regex
import Control.Exception
import Control.Monad
import System.Unix.Directory
import System.Unix.FilePath
import Extra.TIO

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
        timeTaskAndTest (cleanStyle path (commandTask cmd))
        where 
          cmd = "find " ++ outsidePath path ++ " -name '_darcs' -maxdepth 1 -prune | xargs rm -rf"
          cleanStyle path = setStart (Just (" Copy and clean Darcs target to " ++ outsidePath path))
    revision tgt =
        do (_, outh, _, handle) <- lift $ runInteractiveCommand cmd
           revision <- lift (hGetContents outh >>= return . matchRegex (mkRegex "hash='([^']*)'") >>=
                           return . maybe (Left $ "could not find hash field in output of '" ++ cmd ++ "'") (Right . head))
           case revision of
             Left message -> return (Left message)
             Right revision ->
                 do lift $ evaluate (length revision)
                    lift $ waitForProcess handle
                    return . Right $ show tgt ++ "=" ++ revision
        where
          path = topdir (sourceTree tgt)
          cmd = "cd " ++ outsidePath path ++ " && darcs changes --xml-output"
    logText _ revision = "Darcs revision: " ++ maybe "none" id revision

prepareDarcs :: Bool -> FilePath -> Bool -> String -> TIO (Either String Tgt)
prepareDarcs _debug top flush uriAndTag =
    do
      when flush (lift (removeRecursiveSafely dir))
      exists <- lift $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      case tree of 
        Left message -> return (Left message)
        Right tree -> return . Right . Tgt $ Darcs { uri = theUri, tag = theTag, sourceTree = tree }
    where
      verifySource :: FilePath -> TIO (Either String SourceTree)
      verifySource dir =
          -- Note that this logic is the opposite of 'tla changes'
          do result <- runTask (verifyStyle (commandTask ("cd " ++ dir ++ " && darcs whatsnew"))) >>= return . discardOutput
             case result of
               [Result (ExitFailure _)] -> updateSource dir				-- No Changes!
               [Result ExitSuccess] -> removeSource dir >> createSource dir		-- Yes changes
               _ -> error "Internal error 5"
      removeSource :: FilePath -> TIO ()
      removeSource dir = lift $ removeRecursiveSafely dir

      updateSource :: FilePath -> TIO (Either String SourceTree)
      updateSource dir =
          runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && darcs pull --all " ++ theUri))) >>=
          either (return . Left) (const (findSourceTree (rootEnvPath dir))) >>=
          return . either (\ message -> Left $ "Couldn't find sourceTree at " ++ dir ++ ": " ++ message) Right

      createSource :: FilePath -> TIO (Either String SourceTree)
      createSource dir =
          let (parent, _) = splitFileName dir in
          do r1 <- lift (try (createDirectoryIfMissing True parent))
             r2 <- either (return . Left . show) (const (runTaskAndTest (createStyle (commandTask cmd)))) r1
             r3 <- either (return . Left) (const (findSourceTree (rootEnvPath dir))) r2
             let r4 = either (\ message -> Left $ "Couldn't find sourceTree at " ++ dir ++ ": " ++ message) Right r3
             return r4
          where
            cmd = unwords $ ["darcs", "get", "--partial", theUri] ++ maybe [] (\ tag -> [" --tag", "'" ++ tag ++ "'"]) theTag ++ [dir]
{-
          do
            -- Create parent dir and let tla create dir
            let (parent, _) = splitFileName dir
            lift $ createDirectoryIfMissing True parent
            createStyle . systemTask . unwords $ ["darcs", "get", "--partial", theUri] ++ maybe [] (\ tag -> [" --tag", "'" ++ tag ++ "'"]) theTag ++ [dir]
            findSourceTree (rootEnvPath dir) >>= return . maybe (error ("Couldn't find sourceTree at " ++ dir)) id
-}
      verifyStyle = (setStart (Just ("Verifying Darcs source archive " ++ theUri)) .
                     setError Nothing)
      updateStyle = (setStart (Just ("Updating Darcs source for " ++ theUri)) .
                     setError (Just (\ _ -> "updateSource failed")))
      createStyle = (setStart (Just ("Retrieving Darcs source for " ++  theUri)) . 
                     setError (Just (\ _ -> "darcs get failed in " ++ dir)))
      name = snd . splitFileName $ theUri
      (theUri, theTag) =
          case matchRegex (mkRegex "^(.*)(=([^=]*))?$") uriAndTag of
            Just [uri, "", _] -> (uri, Nothing)
            Just [uri, _, tag] -> (uri, Just tag)
            _ -> error "Internal error 6"	-- That regex should always match
      dir = top ++ "/darcs/" ++ name
