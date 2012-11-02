{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Darcs where

import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.List (nub, sort)
import Data.Maybe (catMaybes)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo
import Debian.Repo.Monads.Top (sub)
import Network.URI (URI(..), URIAuth(..), uriToString, parseURI)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (CmdSpec(..), CreateProcess(cwd), showCommandForUser)
import System.Process.Progress (keepStdout, keepResult, timeTask, runProcessF, runProcess)
import System.Unix.Directory
import Text.Regex

documentation = [ "darcs:<string> - a target of this form obtains the source code by running"
                , "darcs get <string>.  If the argument needs to use ssh to reach the darcs"
                , "repository, it is necessary to set up ssh keys to allow access without"
                , "typing a password.  See the --ssh-export option for help doing this." ]

darcsRev :: SourceTree -> P.RetrieveMethod -> IO (Either SomeException String)
darcsRev tree m =
    try (runProcess (\ p -> p {cwd = Just path}) cmd B.empty >>=
         return . matchRegex (mkRegex "hash='([^']*)'") . B.unpack . B.concat . keepStdout) >>= 
    return . either Left (maybe (fail $ "could not find hash field in output of '" ++ showCmd cmd ++ "'")
                                (\ rev -> Right (show m ++ "=" ++ head rev)))
    where
      cmd = RawCommand "darcs" ["changes", "--xml-output"]
      path = topdir tree

showCmd (RawCommand cmd args) = showCommandForUser cmd args
showCmd (ShellCommand cmd) = cmd

prepare :: MonadDeb e m => P.CacheRec -> P.Packages -> String -> m T.Download
prepare cache package theUri =
    sub "darcs" >>= \ base ->
    sub ("darcs" </> sum) >>= \ dir -> liftIO $
    do
      when (P.flushSource (P.params cache)) (removeRecursiveSafely dir)
      exists <- doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      _output <- fixLink base
      return $ T.Download { T.package = package
                          , T.getTop = topdir tree
                          , T.logText =  "Darcs revision: " ++ show (P.spec package)
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ top -> let cmd = "find " ++ top ++ " -name '_darcs' -maxdepth 1 -prune | xargs rm -rf" in
                                       timeTask (runProcessF id (ShellCommand cmd) B.empty)
                          , T.buildWrapper = id
                          }
    where
      verifySource :: FilePath -> IO SourceTree
      verifySource dir =
          -- Note that this logic is the opposite of 'tla changes'
          do result <- runProcess (\ p -> p {cwd = Just dir}) (RawCommand "darcs" ["whatsnew"]) B.empty >>= return . keepResult
             case result of
               (ExitSuccess : _) -> removeSource dir >> createSource dir		-- Yes changes
               _ -> updateSource dir				-- No Changes!
      removeSource :: FilePath -> IO ()
      removeSource dir = removeRecursiveSafely dir

      updateSource :: FilePath -> IO SourceTree
      updateSource dir =
          runProcessF (\ p -> p {cwd = Just dir}) (RawCommand "darcs" ["pull", "--all", renderForDarcs theUri']) B.empty >>
          -- runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && darcs pull --all " ++ renderForDarcs theUri))) >>
          findSourceTree dir

      createSource :: FilePath -> IO SourceTree
      createSource dir =
          let (parent, _) = splitFileName dir in
          do createDirectoryIfMissing True parent
             _output <- runProcessF id cmd B.empty
             findSourceTree dir
          where
            cmd = RawCommand "darcs" (["get", renderForDarcs theUri'] ++ maybe [] (\ tag -> [" --tag", "'" ++ tag ++ "'"]) theTag ++ [dir])
      -- Maybe we should include the "darcs:" in the string we checksum?
      fixLink base =
          let link = base </> name in
          runProcessF id (RawCommand "rm" ["-rf", link]) B.empty >>
          runProcessF id (RawCommand "ln" ["-s", sum, link]) B.empty
      name = snd . splitFileName $ (uriPath theUri')
      sum = show (md5 (B.pack uriAndTag))
      uriAndTag = uriToString id theUri' "" ++ maybe "" (\ tag -> "=" ++ tag) theTag
      theTag = case nub (sort (catMaybes (map (\ flag -> case flag of
                                                           P.DarcsTag s -> Just s
                                                           _ -> Nothing) (P.flags package)))) of
                 [] -> Nothing
                 [x] -> Just x
                 xs -> error ("Conflicting tags for darcs get of " ++ theUri ++ ": " ++ show xs)
      theUri' = mustParseURI theUri

renderForDarcs :: URI -> String
renderForDarcs uri =
    case (uriScheme uri, uriAuthority uri) of
      ("ssh:", Just auth) -> uriUserInfo auth ++ uriRegName auth ++ ":" ++ uriPath uri ++ uriQuery uri ++ uriFragment uri
      (_, _) -> show uri

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Darcs - failed to parse URI: " ++ show s)) id (parseURI s)
