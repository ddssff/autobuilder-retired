module BuildTarget.Svn 
    ( BuildTarget(..)
    , prepareSvn
    , Svn
    , documentation
    ) where

import BuildTarget
import Debian.Types
import Debian.Types.SourceTree
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Linspire.Unix.Directory
import Linspire.Unix.FilePath
import Linspire.Unix.Process
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import System.Directory
import Network.URI
import Debian.Control.ByteString
import Debian.TIO
import Debian.Shell

-- | A Subversion archive
data Svn = Svn URI SourceTree

instance Show Svn where
    show (Svn s _) = "svn:" ++ uriToString id s ""

documentation = [ "svn:<uri> - A target of this form retrieves the source code from"
                , "a subversion repository." ]

svn :: Maybe EnvPath -> [String] -> TIO (Either String [Output])
svn path args =
    lift $ lazyProcess "svn" args (maybe Nothing (Just . outsidePath) path) Nothing [] >>= return . finish
    where
      finish output = case exitCodeOnly output of
                        [ExitSuccess] -> Right output
                        _ -> Left $ "*** FAILURE: svn " ++ concat (intersperse " " args)

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
        cleanStyle path $ runCommandQuietlyTimed cmd
        where
          cmd = "find " ++ outsidePath path ++ " -name .svn -type d -print0 | xargs -0 -r -n1 rm -rf"
          cleanStyle path = setStyle $ setStart (Just (" Copy and clean SVN target to " ++ show path))

    revision (Svn uri tree) =
        svn (Just $ topdir tree) (["info","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo)) >>=
        return . either (const (Left "svn info failed")) readControl
        where
          readControl :: [Output] -> Either String String
          readControl out = 
              case parseControl "svn info" (B.concat (stdoutOnly out)) of
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

prepareSvn ::  Bool -> FilePath -> Bool -> String -> TIO (Either String Tgt)
prepareSvn _debug top flush target =
    do when flush (lift (removeRecursiveSafely dir))
       exists <- lift $ doesDirectoryExist dir
       tree <- if exists then verifySource dir else createSource dir
       case tree of
         Left message -> return $ Left ("No source tree at " ++ show dir ++ ": " ++ message)
         Right tree -> return . Right . Tgt $ Svn uri tree
    where
      verifySource dir =
          svn (Just (rootEnvPath dir)) (["status","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo)) >>=
          either (return . Left) (\ out -> case (stdoutOnly out) ++ (stderrOnly out) of
                                             -- no output == nothing changed
                                             [] -> updateSource dir
                                             -- Failure - error code or output from status means changes have occured
                                             _ ->  removeSource dir >> createSource dir)

      removeSource dir = lift $ removeRecursiveSafely dir

      updateSource dir =
          do
            -- if the original url contained a specific revision, this will do the wrong thing
            updateStyle $ svn (Just (rootEnvPath dir)) (["update","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
            findSourceTree (rootEnvPath dir)

      createSource dir =
          let (parent, _) = splitFileName dir in
          lift (try (createDirectoryIfMissing True parent)) >>=
          either (return . Left . show) (const (lift checkout)) >>=
          either (return . Left) (const (findSourceTree (rootEnvPath dir)))
      checkout :: IO (Either String [Output])
      checkout = lazyProcess "svn" args Nothing Nothing [] >>= return . finish
          where
            args = ([ "co","--no-auth-cache","--non-interactive"] ++ 
                    (username userInfo) ++ (password userInfo) ++ 
                    [ (uriToString (const "") uri ""), dir ])
            finish output = case exitCodeOnly output of
                              [ExitSuccess] -> Right output
                              _ -> Left $ "*** FAILURE: svn " ++ concat (intersperse " " args)
{-
          do
            -- Create parent dir and let tla create dir
            lift $ createDirectoryIfMissing True parent
            createStyle $ runQuietlyTimed "svn" (lazyProcess "svn" ([ "co","--no-auth-cache","--non-interactive"] ++ 
                                                                    (username userInfo) ++ (password userInfo) ++ 
                                                                    [ (uriToString (const "") uri ""), dir ]) Nothing)
            findSourceTree (rootEnvPath dir)
-}
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
