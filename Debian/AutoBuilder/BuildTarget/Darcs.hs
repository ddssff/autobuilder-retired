{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Darcs where

import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.List (nub, sort)
import Data.Maybe (catMaybes)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.PackageFlag as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Repo
import Network.URI (URI(..), URIAuth(..), uriToString, parseURI)
import System.Directory
import System.FilePath
import System.Unix.Directory
import System.Unix.Process
import System.Unix.Progress (lazyCommandF, lazyCommandE, lazyCommandV, timeTask)
import Text.Regex

documentation = [ "darcs:<string> - a target of this form obtains the source code by running"
                , "darcs get <string>.  If the argument needs to use ssh to reach the darcs"
                , "repository, it is necessary to set up ssh keys to allow access without"
                , "typing a password.  See the --ssh-export option for help doing this." ]

darcsRev :: SourceTree -> R.RetrieveMethod -> IO (Either SomeException String)
darcsRev tree m =
    try (lazyCommandE cmd B.empty >>= return . matchRegex (mkRegex "hash='([^']*)'") . B.unpack . fst . collectStdout) >>= 
    return . either Left (maybe (fail $ "could not find hash field in output of '" ++ cmd ++ "'")
                                (\ rev -> Right (show m ++ "=" ++ head rev)))
    where
      cmd = "cd " ++ path ++ " && darcs changes --xml-output"
      path = topdir tree

prepare :: P.CacheRec -> String -> [P.DarcsFlag] -> R.RetrieveMethod -> IO T.Download
prepare cache theUri flags m =
    do
      when (P.flushSource (P.params cache)) (removeRecursiveSafely dir)
      exists <- doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      _output <- fixLink
      rev <- darcsRev tree m >>= either (fail . show) return
      return $ T.Download { T.method = m
                          , T.getTop = topdir tree
                          , T.revision = rev
                          , T.logText =  "Darcs revision: " ++ rev
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ top -> let cmd = "find " ++ top ++ " -name '_darcs' -maxdepth 1 -prune | xargs rm -rf" in
                                       timeTask (lazyCommandF cmd B.empty)
                          , T.buildWrapper = id
                          }
    where
      verifySource :: FilePath -> IO SourceTree
      verifySource dir =
          -- Note that this logic is the opposite of 'tla changes'
          do result <- lazyCommandV ("cd " ++ dir ++ " && darcs whatsnew") B.empty >>= return . exitCodeOnly
             case result of
               ExitFailure _ -> updateSource dir				-- No Changes!
               ExitSuccess -> removeSource dir >> createSource dir		-- Yes changes
      removeSource :: FilePath -> IO ()
      removeSource dir = removeRecursiveSafely dir

      updateSource :: FilePath -> IO SourceTree
      updateSource dir =
          lazyCommandF ("cd " ++ dir ++ " && darcs pull --all " ++ renderForDarcs theUri') B.empty >>
          -- runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && darcs pull --all " ++ renderForDarcs theUri))) >>
          findSourceTree dir

      createSource :: FilePath -> IO SourceTree
      createSource dir =
          let (parent, _) = splitFileName dir in
          do createDirectoryIfMissing True parent
             _output <- lazyCommandF cmd B.empty
             findSourceTree dir
          where
            cmd = unwords $ ["darcs", "get", "--partial", renderForDarcs theUri'] ++ maybe [] (\ tag -> [" --tag", "'" ++ tag ++ "'"]) theTag ++ [dir]
      -- Maybe we should include the "darcs:" in the string we checksum?
      fixLink = let link = base ++ "/" ++ name
                    cmd = "rm -rf " ++ link ++ " && ln -s " ++ sum ++ " " ++ link in
                lazyCommandF cmd B.empty
      base = P.topDir cache ++ "/darcs"
      name = snd . splitFileName $ (uriPath theUri')
      dir = base ++ "/" ++ sum
      sum = show (md5 (B.pack uriAndTag))
      uriAndTag = uriToString id theUri' "" ++ maybe "" (\ tag -> "=" ++ tag) theTag
      theTag = case nub (sort (catMaybes (map (\ flag -> case flag of
                                                           P.DarcsTag s -> Just s
                                                           {- _ -> Nothing -}) flags))) of
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
