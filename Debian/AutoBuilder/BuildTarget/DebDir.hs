{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.DebDir
    ( documentation
    , prepare
    ) where

import Control.Monad.Trans (lift)
import Data.ByteString.Lazy.Char8 (empty)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Tgt (Tgt(Tgt))
import Debian.Changes (logVersion)
import Debian.Version (version)
import Prelude hiding (catch)
import Debian.Repo
import System.Directory
import System.Unix.Progress (lazyCommandF)
-- import Debian.OldShell (commandTask, setStart, runTaskAndTest, setError, runTask)
--import ChangeLog

-- | Get the upstream source from one location, and the debian directory from another
data DebDir = DebDir Tgt Tgt DebianSourceTree

instance Show DebDir where
    show (DebDir t q _) = "deb-dir:(" ++ show t ++ "):(" ++ show q ++ ")"

documentation = [ "deb-dir:(<target>):(<target>) - A target of this form combines two targets,"
                , "where one points to an un-debianized source tree and the other contains"
                , "a debian subdirectory." ]

instance BuildTarget DebDir where
    getTop _ (DebDir _ _ tree) = topdir tree
    cleanTarget params (DebDir upstream debian _) path =
        cleanTarget params upstream path >>
        cleanTarget params debian (path ++ "/debian")
    revision params (DebDir upstream debian _) =
        revision params upstream >>= \ rev ->
        revision params debian >>= \ x -> return ("deb-dir:(" ++ rev ++ "):(" ++ x ++")")
    logText (DebDir _ _ _) revision = "deb-dir revision: " ++ either show id revision
    origTarball c (DebDir u _ _) = origTarball c u

prepare :: P.CacheRec -> Tgt -> Tgt -> AptIOT IO DebDir
prepare cache upstream debian = lift $
    createDirectoryIfMissing True (P.topDir cache ++ "/deb-dir") >>
    copyUpstream >>
    copyDebian >>
    findDebianSourceTree dest >>= \ tree ->
    let tgt = DebDir (Tgt upstream) (Tgt debian) tree in
    -- The upstream and downstream versions must match after the epoch and revision is stripped.
    case mVersion upstream of
      Nothing -> return tgt
      Just upstreamV ->
          let debianV = logVersion (entry tree) in
          case compare (version debianV) (version upstreamV) of
            -- If the debian version is too old it needs to be bumped, this ensures we notice
            -- when a new upstream appears.  We should just modify the changelog directly.
            LT -> error $ show tgt ++ ": version in Debian changelog (" ++ version debianV ++ ") is too old for the upstream (" ++ version upstreamV ++ ")"
            _ -> return tgt
{-
    liftIO  (try (createDirectoryIfMissing True (P.topDir params ++ "/deb-dir"))) >>=
    either (\ (e :: SomeException) -> return . Left . show $ e) (const copyUpstream) >>=
    either (return . Left) (const copyDebian) >>=
    either (return . Left) (const (findDebianSourceTree dest)) >>=
    either (\ message -> return $ Left ("Couldn't find source tree at " ++ show dest ++ ": " ++ message))
           (return . Right . Tgt . DebDir (Tgt upstream) (Tgt debian))
-}
    where
      copyUpstream = lazyCommandF cmd1 empty -- runTaskAndTest (cleanStyle (show upstream) (commandTask cmd1))
      copyDebian = lazyCommandF cmd2 empty -- runTaskAndTest (cleanStyle (show debian) (commandTask cmd2))
      upstreamDir = getTop (P.params cache) upstream
      debianDir = getTop (P.params cache) debian
      dest = P.topDir cache ++ "/deb-dir/" ++ md5sum ("deb-dir:(" ++ show upstream ++ "):(" ++ show debian ++ ")") 
      cmd1 = ("set -x && rsync -aHxSpDt --delete '" ++ upstreamDir ++ "/' '" ++ dest ++ "'")
      cmd2 = ("set -x && rsync -aHxSpDt --delete '" ++ debianDir ++ "/debian' '" ++ dest ++ "/'")
      -- cleanStyle name = setStart (Just (" Prepare deb-dir target " ++ show name))
