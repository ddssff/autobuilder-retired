-- | A Bazaar archive
{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Bzr where

import Control.Exception (SomeException, try, throw)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.List
import Data.Maybe
import qualified Debian.AutoBuilder.Types.CacheRec as P
import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Repo
import Debian.URI
import System.Exit (ExitCode(..))
import System.FilePath (splitFileName)
import System.IO
import System.Process
import System.Unix.Directory
import qualified System.Unix.Process as P
import System.Unix.Progress (timeTask, lazyCommandF)
import System.Unix.QIO (qPutStrLn)
import System.Directory

documentation = [ "bzr:<revision> - A target of this form retrieves the a Bazaar archive with the"
                , "given revision name." ]

prepare :: P.CacheRec -> String -> R.RetrieveMethod -> AptIOT IO Download
prepare cache version method = liftIO $
  do
    when (P.flushSource (P.params cache)) (liftIO (removeRecursiveSafely dir))
    exists <- liftIO $ doesDirectoryExist dir
    tree <- if exists then updateSource dir else createSource dir
    let path = topdir tree
        cmd = "cd " ++ path ++ " && bzr info | awk '/parent branch:/ {print $3}'"
    -- FIXME: this command can take a lot of time, message it
    (_, outh, _, handle) <- liftIO $ runInteractiveCommand cmd
    rev <- hSetBinaryMode outh True >> hGetContents outh >>= return . listToMaybe . lines >>=
           return . maybe (error "no revision info printed by '" ++ cmd ++ "'") id
    code <- waitForProcess handle
    let rev' = case code of
                 ExitSuccess -> "bzr:" ++ rev
                 code -> fail (cmd ++ " -> " ++ show code)
    return $ Download
               { method = method
               , getTop = topdir tree
               , revision = rev'
               , logText = "Bazaar revision: " ++ show rev'
               , mVersion = Nothing
               , origTarball = Nothing
               , cleanTarget = \ top ->
                   do qPutStrLn ("Clean Bazaar target in " ++ top)
                      let cmd = "find '" ++ top ++ "' -name '.bzr' -prune | xargs rm -rf"
                      timeTask (lazyCommandF cmd L.empty)
               , buildWrapper = id }
    where
        -- Tries to update a pre-existant bazaar source tree
        updateSource dir =
            qPutStrLn ("Verifying Bazaar source archive '" ++ dir ++ "'") >>
            try (lazyCommandF cmd L.empty) >>= \ result ->
            case result of
              -- if we fail then the source tree is corrupted, so get a new one
              Left (e :: SomeException) -> qPutStrLn (show e) >> removeSource dir >> createSource dir >> throw e
              -- If we succeed then we try to merge with the parent tree
              Right _output -> mergeSource dir
            where
                cmd   = "cd " ++ dir ++ " && ! `bzr status | grep -q 'modified:'`"
        
        -- computes a diff between this archive and some other parent archive and tries to merge the changes
        mergeSource dir =
            lazyCommandF cmd L.empty >>= \ b ->
            if isInfixOf "Nothing to do." (L.unpack (P.outputOnly b))
            then findSourceTree dir
            else commitSource dir
            where
                cmd   = "cd " ++ dir ++ " && bzr merge"
                -- style = (setStart (Just ("Merging local Bazaar source archive '" ++ dir ++ "' with parent archive")).
                --          setError (Just (\ _ -> "bzr merge failed in '" ++ dir ++ "'")))
        
        -- Bazaar is a distributed revision control system so you must commit to the local source
        -- tree after you merge from some other source tree
        commitSource dir =
            lazyCommandF cmd L.empty >> findSourceTree dir
            where
                cmd   = "cd " ++ dir ++ " && bzr commit -m 'Merged Upstream'"
                -- style = (setStart (Just ("Commiting merge to local Bazaar source archive '" ++ dir ++ "'")) .
                --     setError (Just (\ _ -> "bzr commit failed in '" ++ dir ++ "'")))
        
        removeSource dir = liftIO $ removeRecursiveSafely dir

        createSource dir = do
            -- Create parent dir and let bzr create dir
            let (parent, _) = splitFileName dir
            createDirectoryIfMissing True parent
            _output <- lazyCommandF cmd L.empty
            findSourceTree dir
            where
                cmd   = "bzr branch " ++ version ++ " " ++ dir
                -- style = (setStart (Just ("Retrieving Bazzar source for " ++ version)) .
                --     setError (Just (\ _ -> "bzr branch failed in " ++ dir)))
        uri = mustParseURI version
            where
                mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)
        dir = (P.topDir cache) ++ "/bzr/" ++ show (md5 (L.pack (maybe "" uriRegName (uriAuthority uri) ++ (uriPath uri))))

