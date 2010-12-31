{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Svn 
    ( BuildTarget(..)
    , prepareSvn
    , Svn
    , documentation
    ) where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Debian.AutoBuilder.BuildTarget
import Debian.AutoBuilder.ParamClass (RunClass)
import qualified Debian.AutoBuilder.ParamClass as P
import Debian.Control.ByteString
import Debian.Repo
--import Debian.OldShell (FullTask, runTaskAndTest, processTask, timeTaskAndTest, commandTask, setStart, setError, runTask, processTask)
import Debian.URI
import System.FilePath (splitFileName)
import System.Unix.Directory
import System.Unix.Process
import System.Unix.Progress (timeTask, lazyCommandF, lazyProcessF, lazyProcessE)
import System.Directory

-- | A Subversion archive
data Svn = Svn URI SourceTree

instance Show Svn where
    show (Svn s _) = "svn:" ++ uriToString id s ""

documentation = [ "svn:<uri> - A target of this form retrieves the source code from"
                , "a subversion repository." ]

{-
svn :: (FullTask -> FullTask) -> Maybe FilePath -> [String] -> IO [Output]
svn style path args =
    runTaskAndTest (style task)
    where
      task = processTask "svn" args path Nothing
-}

svn :: [String] -> IO [Output]
svn args = lazyProcessF "svn" args Nothing Nothing L.empty

username userInfo = 
    let un = takeWhile (/= ':') userInfo in
    if null un
    then []
    else ["--username", unEscapeString un]
     
password userInfo =
    let pw = takeWhile (/= '@') . dropWhile (== ':') . dropWhile (/= ':') $ userInfo in
    if null pw
    then []
    else ["--password",unEscapeString pw]

instance BuildTarget Svn where
    getTop _ (Svn _ tree) = topdir tree
    -- We should recursively find and remove all the .svn directories in |dir source|
    cleanTarget _ (Svn _ _) path =
        timeTask (lazyCommandF cmd L.empty)
        -- timeTaskAndTest (cleanStyle path (commandTask cmd))
        where
          cmd = "find " ++ path ++ " -name .svn -type d -print0 | xargs -0 -r -n1 rm -rf"
          -- cleanStyle path = setStart (Just (" Copy and clean SVN target to " ++ path))

    revision _ (Svn uri tree) =
        svn (["info","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo)) >>=
        -- svn id (Just $ topdir tree) (["info","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo)) >>=
        return . readControl
        where
          readControl :: [Output] -> String
          readControl out = 
              case parseControl "svn info" (B.concat (L.toChunks (stdoutOnly out))) of
                (Right (Control (c:_))) ->
                    -- JAS, I don't know why I did not just use the uri that was passed in
                    case (lookupP "URL" c, lookupP "Revision" c) of
                      (Just (Field (_, url)), Just (Field (_, revision))) ->
                          "svn:" ++ (B.unpack (stripWS url)) ++"@" ++ (B.unpack (stripWS revision))
                      _ -> fail "Failed to find URL and/or Revision fields in svn info"
                (Right (Control [])) -> fail $ "svn info did not appear to produce any output"
                Left e -> fail $ "Failed to parse svn info\n" ++ show e
          userInfo = maybe "" uriUserInfo (uriAuthority uri)
{-        
        do
          -- FIXME: this command can take a lot of time, message it
          (out, _) <- svn (Just $ topdir tree) (["info","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
          case parseControl "svn info" (B.concat (stdoutOnly out)) of
            (Right (Control (c:_))) ->
                case (lookupP "URL" c, lookupP "Revision" c) of -- JAS, I don't know why I did not just use the uri that was passed in
                  (Just (Field (_, url)), Just (Field (_, revision))) ->
                      return $ Just $ "svn:" ++ (B.unpack (stripWS url)) ++"@" ++ (B.unpack (stripWS revision))
                  _ -> error $ "Failed to find URL and/or Revision fields in svn info"
            (Right (Control [])) -> error $ "svn info did not appear to produce any output"
            Left e -> error $ "Failed to parse svn info\n" ++ show e
        where
          userInfo = maybe "" uriUserInfo (uriAuthority uri)
-}
    logText (Svn _ _) revision = "SVN revision: " ++ either show id revision

prepareSvn ::  (RunClass p) => p -> String -> IO Tgt
prepareSvn params target =
    do when (P.flushSource params) (liftIO (removeRecursiveSafely dir))
       exists <- liftIO $ doesDirectoryExist dir
       tree <- if exists then verifySource dir else createSource dir
       return . Tgt $ Svn uri tree
    where
      verifySource dir =
          svn (["status","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo)) >>= \ out ->
          case L.append (stdoutOnly out) (stderrOnly out) == L.empty of
            -- no output == nothing changed
            True -> updateSource dir
            -- Failure - error code or output from status means changes have occured
            False ->  removeSource dir >> createSource dir

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          do
            -- if the original url contained a specific revision, this will do the wrong thing
            svn (["update","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
            findSourceTree dir

      createSource dir =
          let (parent, _) = splitFileName dir in
          liftIO (createDirectoryIfMissing True parent) >>
          checkout >>
          findSourceTree dir
      checkout :: IO (Either String [Output])
      --checkout = svn createStyle args 
      checkout = lazyProcessE "svn" args Nothing Nothing L.empty >>= return . finish
          where
            args = ([ "co","--no-auth-cache","--non-interactive"] ++ 
                    (username userInfo) ++ (password userInfo) ++ 
                    [ (uriToString (const "") uri ""), dir ])
            finish output = case exitCodeOnly output of
                              ExitSuccess -> Right output
                              _ -> Left $ "*** FAILURE: svn " ++ concat (intersperse " " args)
{-
      verifyStyle = (setStart (Just ("Verifying SVN source archive " ++ uriToString' uri)) .
                     setError (Just (\ _ -> "SVN diff failed in" ++ dir)))
      updateStyle = (setStart (Just ("Updating SVN source for " ++ uriToString' uri)) .
                     setError (Just (\ _ -> "updateSource failed")))
      createStyle = (setStart (Just ("Retrieving SVN source for " ++ uriToString' uri)) .
                     setError (Just (\ _ -> "svn co failed in " ++ dir)))
-}
      uri = mustParseURI target
      userInfo = maybe "" uriUserInfo (uriAuthority uri)
      dir = P.topDir params ++ "/svn/" ++ md5sum (maybe "" uriRegName (uriAuthority uri) ++ (uriPath uri))

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)
