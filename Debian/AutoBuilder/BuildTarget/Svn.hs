{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Svn 
    ( BuildTarget(..)
    , prepareSvn
    , Svn
    , documentation
    ) where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import Debian.AutoBuilder.BuildTarget
import Debian.AutoBuilder.ParamClass (RunClass)
import qualified Debian.AutoBuilder.ParamClass as P
import Debian.Control.ByteString
import Debian.Repo
import Debian.Shell
import Debian.URI
import Extra.CIO
import System.FilePath (splitFileName)
import System.Unix.Directory
import System.Unix.Process
import System.Directory

-- | A Subversion archive
data Svn = Svn URI SourceTree

instance Show Svn where
    show (Svn s _) = "svn:" ++ uriToString id s ""

documentation = [ "svn:<uri> - A target of this form retrieves the source code from"
                , "a subversion repository." ]

svn :: CIO m => (FullTask -> FullTask) -> Maybe FilePath -> [String] -> m (Either String [Output])
svn style path args =
    runTaskAndTest (style task) >>= return . either (Left . (("*** FAILURE: " ++ showCommand task ++ ": ") ++)) Right
    where
      task = processTask "svn" args path Nothing

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
        timeTaskAndTest (cleanStyle path (commandTask cmd))
        where
          cmd = "find " ++ path ++ " -name .svn -type d -print0 | xargs -0 -r -n1 rm -rf"
          cleanStyle path = setStart (Just (" Copy and clean SVN target to " ++ path))

    revision _ (Svn uri tree) =
        svn id (Just $ topdir tree) (["info","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo)) >>=
        return . either (const (Left "svn info failed")) readControl
        where
          readControl :: [Output] -> Either String String
          readControl out = 
              case parseControl "svn info" (B.concat (L.toChunks (stdoutOnly out))) of
                (Right (Control (c:_))) ->
                    -- JAS, I don't know why I did not just use the uri that was passed in
                    case (lookupP "URL" c, lookupP "Revision" c) of
                      (Just (Field (_, url)), Just (Field (_, revision))) ->
                          Right $ "svn:" ++ (B.unpack (stripWS url)) ++"@" ++ (B.unpack (stripWS revision))
                      _ -> Left $ "Failed to find URL and/or Revision fields in svn info"
                (Right (Control [])) -> Left $ "svn info did not appear to produce any output"
                Left e -> Left $ "Failed to parse svn info\n" ++ show e
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
    logText (Svn _ _) revision = "SVN revision: " ++ maybe "none" id revision

prepareSvn ::  (RunClass p, CIO m) => p -> String -> m (Either String Tgt)
prepareSvn params target =
    do when (P.flushSource params) (liftIO (removeRecursiveSafely dir))
       exists <- liftIO $ doesDirectoryExist dir
       tree <- if exists then verifySource dir else createSource dir
       case tree of
         Left message -> return $ Left ("No source tree at " ++ show dir ++ ": " ++ message)
         Right tree -> return . Right . Tgt $ Svn uri tree
    where
      verifySource dir =
          svn verifyStyle (Just dir) (["status","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo)) >>=
          either (return . Left) (\ out -> case L.append (stdoutOnly out) (stderrOnly out) == L.empty of
                                             -- no output == nothing changed
                                             True -> updateSource dir
                                             -- Failure - error code or output from status means changes have occured
                                             False ->  removeSource dir >> createSource dir)

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          do
            -- if the original url contained a specific revision, this will do the wrong thing
            svn updateStyle (Just dir) (["update","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
            findSourceTree dir

      createSource dir =
          let (parent, _) = splitFileName dir in
          liftIO (try (createDirectoryIfMissing True parent)) >>=
          either (\ (e :: SomeException) -> return . Left . show $ e) (const checkout) >>=
          either (return . Left) (const (findSourceTree dir))
      checkout :: CIO m => m (Either String [Output])
      --checkout = svn createStyle args 
      checkout = runTask (createStyle (processTask "svn" args Nothing Nothing)) >>= return . finish
          where
            args = ([ "co","--no-auth-cache","--non-interactive"] ++ 
                    (username userInfo) ++ (password userInfo) ++ 
                    [ (uriToString (const "") uri ""), dir ])
            finish output = case exitCodeOnly output of
                              [ExitSuccess] -> Right output
                              _ -> Left $ "*** FAILURE: svn " ++ concat (intersperse " " args)
      verifyStyle = (setStart (Just ("Verifying SVN source archive " ++ uriToString' uri)) .
                     setError (Just (\ _ -> "SVN diff failed in" ++ dir)))
      updateStyle = (setStart (Just ("Updating SVN source for " ++ uriToString' uri)) .
                     setError (Just (\ _ -> "updateSource failed")))
      createStyle = (setStart (Just ("Retrieving SVN source for " ++ uriToString' uri)) .
                     setError (Just (\ _ -> "svn co failed in " ++ dir)))
      uri = mustParseURI target
      userInfo = maybe "" uriUserInfo (uriAuthority uri)
      dir = P.topDir params ++ "/svn/" ++ escapeForMake (maybe "" uriRegName (uriAuthority uri)) ++ (uriPath uri)

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)
