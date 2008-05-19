module BuildTarget.Bzr where

import Debian.Repo.Types
import Debian.Repo.SourceTree
import Debian.Shell
import Debian.URI

import Control.Monad.Trans
import BuildTarget
import System.IO
import Control.Monad
import System.Process
import System.Unix.Directory
import System.Unix.FilePath
import Data.Maybe
import Data.List
import System.Directory
import Extra.TIO
import qualified Data.ByteString.Lazy.Char8 as L
import qualified System.Unix.Process as P

-- | A Bazaar archive
data Bzr = Bzr String SourceTree

instance Show Bzr where
    show (Bzr s _) = "bzr:" ++ s

documentation = [ "bzr:<revision> - A target of this form retrieves the a Bazaar archive with the"
                , "given revision name." ]

instance BuildTarget Bzr where
    getTop (Bzr _ tree) = topdir tree
    cleanTarget (Bzr _ _) path =
        timeTaskAndTest (cleanStyle path (commandTask cmd))
        where
          cmd = "find '" ++ outsidePath path ++ "' -name '.bzr' -prune | xargs rm -rf"
          cleanStyle path = setStart (Just ("Clean Bazzar target in " ++ outsidePath path))

    revision (Bzr _ tree) =
        do let path = topdir tree
               cmd = "cd " ++ outsidePath path ++ " && bzr info | awk '/parent branch:/ {print $3}'"
           -- FIXME: this command can take a lot of time, message it
           (_, outh, _, handle) <- lift $ runInteractiveCommand cmd
           revision <- lift (hGetContents outh >>= return . listToMaybe . lines) >>=
                       return . maybe (error "no revision info printed by '" ++ cmd ++ "'") id
           lift $ waitForProcess handle
           return . Right $ "bzr:" ++ revision

    logText (Bzr _ _) revision = "Bazaar revision: " ++ maybe "none" id revision

prepareBzr :: FilePath -> Bool -> String -> TIO (Either String Tgt)
prepareBzr top flush version = do
    when flush (lift (removeRecursiveSafely dir))
    exists <- lift $ doesDirectoryExist dir
    tree <- if exists then updateSource dir else createSource dir
    case tree of
        Left message -> return . Left $ "failed to find source tree at " ++ dir ++ ": " ++ message
        Right tree -> return . Right . Tgt $ Bzr version tree
    where
        -- Tries to update a pre-existant bazaar source tree
        updateSource dir =
            runTaskAndTest (style (commandTask cmd)) >>= \result ->
            case result of
                -- if we fail then the source tree is corrupted, so get a new one
                Left message -> vPutStrBl 0 message  >> removeSource dir >> createSource dir
                -- If we succeed then we try to merge with the parent tree
                Right output -> mergeSource dir
            where
                cmd   = "cd " ++ dir ++ " && ! `bzr status | grep -q 'modified:'`"
                style = (setStart (Just ("Verifying Bazaar source archive '" ++ dir ++ "'")) .
                    setError (Just (\ _ -> "bzr status dirty in '" ++ dir ++ "'")))
        
        -- computes a diff between this archive and some other parent archive and tries to merge the changes
        mergeSource dir =
            runTaskAndTest (style (commandTask cmd)) >>= \result ->
            case result of
                Left  a -> return (Left a)
                Right b -> if isInfixOf "Nothing to do." (L.unpack (P.outputOnly b))
                           then findSourceTree (rootEnvPath dir)
                           else commitSource dir
            where
                cmd   = "cd " ++ dir ++ " && bzr merge"
                style = (setStart (Just ("Merging local Bazaar source archive '" ++ dir ++ "' with parent archive")).
                    setError (Just (\ _ -> "bzr merge failed in '" ++ dir ++ "'")))
        
        -- Bazaar is a distributed revision control system so you must commit to the local source
        -- tree after you merge from some other source tree
        commitSource dir =
            runTaskAndTest (style (commandTask cmd)) >>= \result ->
            case result of
                Left  a -> return (Left a)
                Right b -> findSourceTree (rootEnvPath dir)
            where
                cmd   = "cd " ++ dir ++ " && bzr commit -m 'Merged Upstream'"
                style = (setStart (Just ("Commiting merge to local Bazaar source archive '" ++ dir ++ "'")) .
                    setError (Just (\ _ -> "bzr commit failed in '" ++ dir ++ "'")))
        
        removeSource dir = lift $ removeRecursiveSafely dir

        createSource dir = do
            -- Create parent dir and let bzr create dir
            let (parent, _) = splitFileName dir
            lift $ createDirectoryIfMissing True parent
            runTaskAndTest (style (commandTask (cmd)))
            findSourceTree (rootEnvPath dir)
            where
                cmd   = "bzr branch " ++ version ++ " " ++ dir
                style = (setStart (Just ("Retrieving Bazzar source for " ++ version)) .
                    setError (Just (\ _ -> "bzr branch failed in " ++ dir)))
        uri = mustParseURI version
            where
                mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)
        dir = top ++ "/bzr/" ++ escapeForMake (maybe "" uriRegName (uriAuthority uri)) ++ (uriPath uri)

