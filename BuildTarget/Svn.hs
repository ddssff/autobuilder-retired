module BuildTarget.Svn 
    ( BuildTarget(..)
    , prepareSvn
    , Svn
    , documentation
    ) where

import BuildTarget
import Debian.Types
import Debian.Types.SourceTree
import System.Time
import Control.Monad
import Linspire.Unix.Directory
import Linspire.Unix.FilePath
import Linspire.Unix.Process
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import System.Directory
import Network.URI
import Debian.Control.ByteString
import Debian.IO
import DryRun

-- | A Subversion archive
data Svn = Svn URI SourceTree

instance Show Svn where
    show (Svn s _) = "svn:" ++ uriToString id s ""

documentation = [ "svn:<uri> - A target of this form retrieves the source code from"
                , "a subversion repository." ]

svn :: Maybe EnvPath -> [String] -> AptIO TaskSuccess
svn path args = systemProcess "svn" args (maybe Nothing (Just . outsidePath) path) Nothing

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
    getTop (Svn _ tree) = topdir tree
    -- We should recursively find and remove all the .svn directories in |dir source|
    cleanTarget (Svn _ _) path = 
        do let cmd = "find " ++ outsidePath path ++ " -name .svn -type d -print0 | xargs -0 -r -n1 rm -rf"

           dr' (return noTimeDiff) (cleanStyle path $ systemTask_ cmd)
           return ()
        where cleanStyle path = setStyle $ setStart (Just (" Copy and clean SVN target to " ++ show path))

    revision (Svn uri tree) =
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

    logText (Svn _ _) revision = "SVN revision: " ++ maybe "none" id revision

prepareSvn ::  Bool -> FilePath -> Bool -> String -> AptIO Tgt
prepareSvn _debug top flush target =
    do
      when flush (removeRecursiveSafelyDR dir)
      exists <- io $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      return $ Tgt $ Svn uri (maybe (error ("No source tree at " ++ show dir)) id tree)
    where
      verifySource dir =
          do (out,_) <- svn (Just (rootEnvPath dir)) (["status","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
             case (stdoutOnly out) ++ (stderrOnly out) of
               -- no output == nothing changed
               [] -> updateSource dir
               -- Failure - error code or output from status means changes have occured
               _ ->  removeSource dir >> createSource dir

      removeSource dir = io $ removeRecursiveSafely dir

      updateSource dir =
          do
            -- if the original url contained a specific revision, this will do the wrong thing
            updateStyle $ svn (Just (rootEnvPath dir)) (["update","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
            findSourceTree (rootEnvPath dir)

      createSource dir =
          do
            -- Create parent dir and let tla create dir
            let (parent, _) = splitFileName dir
            io $ createDirectoryIfMissing True parent
            createStyle $ systemProcess "svn" ([ "co","--no-auth-cache","--non-interactive"] ++ 
                                               (username userInfo) ++ (password userInfo) ++ 
                                               [ (uriToString (const "") uri ""), dir ]) Nothing Nothing
            findSourceTree (rootEnvPath dir)
      _verifyStyle = setStyle (setStart (Just ("Verifying SVN source archive " ++ (show uri))) .
                               setError (Just ("SVN diff failed in" ++ dir)) {- . Output Indented-})
      updateStyle = setStyle (setStart (Just ("Updating SVN source for " ++ (show uri))) .
                              setError (Just "updateSource failed") {-, Output Indented-} )
      createStyle = setStyle (setStart (Just ("Retrieving SVN source for " ++ (show uri))) .
                              setError (Just ("svn co failed in " ++ dir)) .
                              setEcho True)
      uri = mustParseURI target
      userInfo = maybe "" uriUserInfo (uriAuthority uri)
      dir = top ++ "/svn/" ++ escapeForMake (maybe "" uriRegName (uriAuthority uri)) ++ (uriPath uri)

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)
