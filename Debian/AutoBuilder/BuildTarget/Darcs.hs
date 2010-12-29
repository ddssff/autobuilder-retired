{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Darcs where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as B
import Debian.AutoBuilder.BuildTarget
import Debian.AutoBuilder.ParamClass (RunClass)
import qualified Debian.AutoBuilder.ParamClass as P
import Debian.Repo
--import Debian.OldShell (timeTaskAndTest, commandTask, setStart, setError, runTask)
import Network.URI (URI(..), URIAuth(..), parseURI)
import System.Directory
import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.Unix.Directory
import System.Unix.Process
import System.Unix.Progress (lazyCommandF, lazyCommandV, timeTask)
import Text.Regex

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
    getTop _ t = topdir (sourceTree t)
    cleanTarget _ _ path =
        timeTask (lazyCommandF cmd B.empty)
        -- timeTaskAndTest (cleanStyle path (commandTask cmd))
        where 
          cmd = "find " ++ path ++ " -name '_darcs' -maxdepth 1 -prune | xargs rm -rf"
          -- cleanStyle path = setStart (Just (" Copy and clean Darcs target to " ++ path))
    revision _ tgt =
        do rev <- liftIO (lazyCommand cmd B.empty >>=
                          return . matchRegex (mkRegex "hash='([^']*)'") . B.unpack . fst . collectStdout >>= 
                          return . maybe (Left $ "could not find hash field in output of '" ++ cmd ++ "'") (Right . head))
           case rev of
             Left message -> fail message
             Right rev' ->
                 do -- Nastygram to self: thanks for not documenting this
                    -- liftIO $ evaluate (length rev')
                    return $ show tgt ++ "=" ++ rev'
        where
          path = topdir (sourceTree tgt)
          cmd = "cd " ++ path ++ " && darcs changes --xml-output"
    logText _ revision = "Darcs revision: " ++ either show id revision

prepareDarcs :: (RunClass p) => p -> String -> IO Tgt
prepareDarcs params uriAndTag =
    do
      when (P.flushSource params) (removeRecursiveSafely dir)
      exists <- doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      liftIO fixLink
      return . Tgt $ Darcs { uri = theUri, tag = theTag, sourceTree = tree }
    where
      verifySource :: FilePath -> IO SourceTree
      verifySource dir =
          -- Note that this logic is the opposite of 'tla changes'
          do result <- lazyCommandV ("cd " ++ dir ++ " && darcs whatsnew") B.empty >>= return . exitCodeOnly
             case result of
               ExitFailure _ -> updateSource dir				-- No Changes!
               ExitSuccess -> removeSource dir >> createSource dir		-- Yes changes
      removeSource :: FilePath -> IO ()
      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource :: FilePath -> IO SourceTree
      updateSource dir =
          lazyCommandF ("cd " ++ dir ++ " && darcs pull --all " ++ renderForDarcs theUri) B.empty >>
          -- runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && darcs pull --all " ++ renderForDarcs theUri))) >>
          findSourceTree dir

      createSource :: FilePath -> IO SourceTree
      createSource dir =
          let (parent, _) = splitFileName dir in
          do r1 <- liftIO (createDirectoryIfMissing True parent)
             r2 <- lazyCommandF cmd B.empty
             -- r2 <- runTaskAndTest (createStyle (commandTask cmd))
             findSourceTree dir
{-             
          do r1 <- liftIO (try (createDirectoryIfMissing True parent))
             r2 <- either (\ (e :: SomeException) -> return . Left . show $ e)
                          (const (runTaskAndTest (createStyle (commandTask cmd)))) r1
             r3 <- either (return . Left) (const (findSourceTree dir)) r2
             let r4 = either (\ message -> Left $ "Couldn't find sourceTree at " ++ dir ++ ": " ++ message) Right r3
             return r4
-}
          where
            cmd = unwords $ ["darcs", "get", "--partial", renderForDarcs theUri] ++ maybe [] (\ tag -> [" --tag", "'" ++ tag ++ "'"]) theTag ++ [dir]
{-
          do
            -- Create parent dir and let tla create dir
            let (parent, _) = splitFileName dir
            liftIO $ createDirectoryIfMissing True parent
            createStyle . systemTask . unwords $ ["darcs", "get", "--partial", theUri] ++ maybe [] (\ tag -> [" --tag", "'" ++ tag ++ "'"]) theTag ++ [dir]
            findSourceTree (rootEnvPath dir) >>= return . maybe (error ("Couldn't find sourceTree at " ++ dir)) id
-}
{-
      verifyStyle = (setStart (Just ("Verifying Darcs source archive " ++ theUri)) .
                     setError Nothing)
      updateStyle = (setStart (Just ("Updating Darcs source for " ++ theUri)) .
                     setError (Just (\ _ -> "updateSource failed")))
      createStyle = (setStart (Just ("Retrieving Darcs source for " ++  theUri)) . 
                     setError (Just (\ _ -> "darcs get failed in " ++ dir)))
-}
      name = snd . splitFileName $ theUri
      (theUri, theTag) =
          case matchRegex (mkRegex "^(.*)(=([^=]*))?$") uriAndTag of
            Just [uri, "", _] -> (uri, Nothing)
            Just [uri, _, tag] -> (uri, Just tag)
            _ -> error "Internal error 6"	-- That regex should always match
      -- Maybe we should include the "darcs:" in the string we checksum?
      fixLink = let link = base ++ "/" ++ name
                    cmd = "rm -rf " ++ link ++ " && ln -s " ++ sum ++ " " ++ link in
                lazyCommandF cmd B.empty
      dir = base ++ "/" ++ sum
      sum = md5sum uriAndTag
      base = P.topDir params ++ "/darcs"

renderForDarcs :: String -> String
renderForDarcs s =
    case parseURI s of
      Nothing -> s
      Just uri ->
          case (uriScheme uri, uriAuthority uri) of
            ("ssh:", Just auth) -> uriUserInfo auth ++ uriRegName auth ++ ":" ++ uriPath uri ++ uriQuery uri ++ uriFragment uri
            (_, _) -> show uri
