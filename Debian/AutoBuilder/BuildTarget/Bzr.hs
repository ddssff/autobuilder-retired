{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Bzr where

import Control.Exception (SomeException, try, throw)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import Debian.AutoBuilder.BuildTarget.Common (BuildTarget(..), md5sum)
import qualified Debian.AutoBuilder.Params as P
import Debian.Repo
import Debian.URI
import System.Exit (ExitCode(..))
import System.FilePath (splitFileName)
import System.IO
import System.Process
import System.Unix.Directory
import qualified System.Unix.Process as P
import System.Unix.Progress (timeTask, lazyCommandF, ePutStrLn, qPutStrLn)
import System.Directory

-- | A Bazaar archive
data Bzr = Bzr String SourceTree

instance Show Bzr where
    show (Bzr s _) = "bzr:" ++ s

documentation = [ "bzr:<revision> - A target of this form retrieves the a Bazaar archive with the"
                , "given revision name." ]

instance BuildTarget Bzr where
    getTop _ (Bzr _ tree) = topdir tree
    cleanTarget _ (Bzr _ _) path =
        ePutStrLn ("Clean Bazzar target in " ++ path) >> 
        timeTask (lazyCommandF cmd L.empty)
        where
          cmd = "find '" ++ path ++ "' -name '.bzr' -prune | xargs rm -rf"

    revision _ (Bzr _ tree) =
        do let path = topdir tree
               cmd = "cd " ++ path ++ " && bzr info | awk '/parent branch:/ {print $3}'"
           -- FIXME: this command can take a lot of time, message it
           (_, outh, _, handle) <- liftIO $ runInteractiveCommand cmd
           rev <- hSetBinaryMode outh True >> hGetContents outh >>= return . listToMaybe . lines >>=
                       return . maybe (error "no revision info printed by '" ++ cmd ++ "'") id
           code <- waitForProcess handle
           case code of
             ExitSuccess -> return $ "bzr:" ++ rev
             code -> fail (cmd ++ " -> " ++ show code)

    logText (Bzr _ _) revision = "Bazaar revision: " ++ either show id revision

prepare' = undefined

prepare :: P.CacheRec -> String -> AptIOT IO Bzr
prepare cache version = liftIO $
  do
    when (P.flushSource (P.params cache)) (liftIO (removeRecursiveSafely dir))
    exists <- liftIO $ doesDirectoryExist dir
    tree <- if exists then updateSource dir else createSource dir
    return $ Bzr version tree
    where
        -- Tries to update a pre-existant bazaar source tree
        updateSource dir =
            ePutStrLn ("Verifying Bazaar source archive '" ++ dir ++ "'") >>
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
            output <- lazyCommandF cmd L.empty
            findSourceTree dir
            where
                cmd   = "bzr branch " ++ version ++ " " ++ dir
                -- style = (setStart (Just ("Retrieving Bazzar source for " ++ version)) .
                --     setError (Just (\ _ -> "bzr branch failed in " ++ dir)))
        uri = mustParseURI version
            where
                mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)
        dir = (P.topDir cache) ++ "/bzr/" ++ md5sum (maybe "" uriRegName (uriAuthority uri) ++ (uriPath uri))

