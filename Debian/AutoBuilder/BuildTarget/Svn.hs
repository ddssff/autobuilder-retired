{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Svn 
    ( prepare
    , documentation
    ) where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.List
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Control.ByteString
import Debian.Repo
import Debian.URI
import System.FilePath (splitFileName)
import System.Unix.Directory
import System.Unix.Process
import System.Unix.Progress (timeTask, lazyCommandF, lazyProcessF, lazyProcessE)
import System.Directory

-- | A Subversion archive
-- data Svn = Svn URI SourceTree R.RetrieveMethod

documentation = [ "svn:<uri> - A target of this form retrieves the source code from"
                , "a subversion repository." ]

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

prepare :: P.CacheRec -> String -> R.RetrieveMethod -> AptIOT IO T.Download
prepare cache uri m = liftIO $
    do when (P.flushSource (P.params cache)) (liftIO (removeRecursiveSafely dir))
       exists <- liftIO $ doesDirectoryExist dir
       tree <- if exists then verifySource dir else createSource dir
       rev <- let readControl :: [Output] -> String
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
                  userInfo = maybe "" uriUserInfo (uriAuthority uri') in
              svn (["info","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo)) >>=
              return . readControl
       return $ T.Download { T.method = m
                           , T.getTop = topdir tree
                           , T.revision = rev
                           , T.logText =  "SVN revision: " ++ rev
                           , T.mVersion = Nothing
                           , T.origTarball = Nothing
                           , T.cleanTarget =
                               \ path -> 
                                   let cmd = "find " ++ path ++ " -name .svn -type d -print0 | xargs -0 -r -n1 rm -rf" in
                                   timeTask (lazyCommandF cmd L.empty)
                           , T.buildWrapper = id
                           }
    where
      uri' = mustParseURI uri
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
            _output <- svn (["update","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
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
                    [ uri, dir ])
            finish output = case exitCodeOnly output of
                              ExitSuccess -> Right output
                              _ -> Left $ "*** FAILURE: svn " ++ concat (intersperse " " args)
      userInfo = maybe "" uriUserInfo (uriAuthority uri')
      dir = P.topDir cache ++ "/svn/" ++ show (md5 (L.pack (maybe "" uriRegName (uriAuthority uri') ++ (uriPath uri'))))

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Svn - parse failure: " ++ show s)) id (parseURI s)
