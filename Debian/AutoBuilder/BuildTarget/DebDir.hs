{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.DebDir
    ( documentation
    , prepare
    ) where

import Control.Monad.Trans (lift)
import Data.ByteString.Lazy.Char8 (empty, pack)
import Data.Digest.Pure.MD5 (md5)
import Data.Version (showVersion)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Changes (logVersion)
import Debian.Version (version)
import Prelude hiding (catch)
import Debian.Repo
import System.Directory
import System.Unix.Progress (lazyCommandF)

documentation = [ "deb-dir:(<target>):(<target>) - A target of this form combines two targets,"
                , "where one points to an un-debianized source tree and the other contains"
                , "a debian subdirectory." ]

prepare :: P.CacheRec -> P.RetrieveMethod -> [P.PackageFlag] -> T.Download -> T.Download -> AptIOT IO T.Download
prepare cache m flags upstream debian = lift $
    createDirectoryIfMissing True (P.topDir cache ++ "/deb-dir") >>
    copyUpstream >>
    copyDebian >>
    findDebianSourceTree dest >>= \ tree ->
    let tgt = T.Download {
                T.method = m
              , T.flags = flags
              , T.getTop = topdir tree
              , T.logText = "deb-dir revision: " ++ show m
              , T.mVersion = Nothing
              , T.origTarball = T.origTarball upstream
              , T.cleanTarget = \ _ -> return ([], 0)
              , T.buildWrapper = id
              } in
    -- The upstream and downstream versions must match after the epoch and revision is stripped.
    case T.mVersion upstream of
      Nothing -> return tgt
      Just upstreamV ->
          let debianV = logVersion (entry tree) in
          case compare (version debianV) (showVersion upstreamV) of
            -- If the debian version is too old it needs to be bumped, this ensures we notice
            -- when a new upstream appears.  We should just modify the changelog directly.
            LT -> error $ show m ++ ": version in Debian changelog (" ++ version debianV ++ ") is too old for the upstream (" ++ showVersion upstreamV ++ ")"
            _ -> return tgt
    where
      copyUpstream = lazyCommandF cmd1 empty
      copyDebian = lazyCommandF cmd2 empty
      upstreamDir = T.getTop upstream
      debianDir = T.getTop debian
      dest = P.topDir cache ++ "/deb-dir/" ++ show (md5 (pack (show m)))
      cmd1 = ("set -x && rsync -aHxSpDt --delete '" ++ upstreamDir ++ "/' '" ++ dest ++ "'")
      cmd2 = ("set -x && rsync -aHxSpDt --delete '" ++ debianDir ++ "/debian' '" ++ dest ++ "/'")
