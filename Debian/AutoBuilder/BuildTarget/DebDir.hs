{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.DebDir
    ( documentation
    , prepare
    ) where

import Control.Monad.Trans (lift)
import Data.ByteString.Lazy.Char8 (empty)
import Data.Version (showVersion)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.BuildTarget.Temp as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Changes (logVersion)
import Debian.Version (version)
import Prelude hiding (catch)
import Debian.Repo
import System.Directory
import System.Unix.Progress (lazyCommandF)

-- | Get the upstream source from one location, and the debian directory from another
-- data DebDir = forall a b. (Download a, Download b) => DebDir a b DebianSourceTree R.RetrieveMethod

documentation = [ "deb-dir:(<target>):(<target>) - A target of this form combines two targets,"
                , "where one points to an un-debianized source tree and the other contains"
                , "a debian subdirectory." ]

prepare :: P.CacheRec -> T.Download -> T.Download -> R.RetrieveMethod -> AptIOT IO T.Download
prepare cache upstream debian m = lift $
    createDirectoryIfMissing True (P.topDir cache ++ "/deb-dir") >>
    copyUpstream >>
    copyDebian >>
    findDebianSourceTree dest >>= \ tree ->
    let rev = "deb-dir:(" ++ revision upstream ++ "):(" ++ revision debian ++")"
        tgt = T.Download {
                T.method = m
              , T.getTop = topdir tree
              , T.revision = rev
              , T.logText = "deb-dir revision: " ++ rev
              , T.mVersion = Nothing
              , T.origTarball = origTarball upstream
              , T.cleanTarget = \ _ -> return ([], 0)
              , T.buildWrapper = id
              } in
    -- The upstream and downstream versions must match after the epoch and revision is stripped.
    case mVersion upstream of
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
      upstreamDir = getTop upstream
      debianDir = getTop debian
      dest = P.topDir cache ++ "/deb-dir/" ++ md5sum ("deb-dir:(" ++ show (method upstream) ++ "):(" ++ show (method debian) ++ ")") 
      cmd1 = ("set -x && rsync -aHxSpDt --delete '" ++ upstreamDir ++ "/' '" ++ dest ++ "'")
      cmd2 = ("set -x && rsync -aHxSpDt --delete '" ++ debianDir ++ "/debian' '" ++ dest ++ "/'")
