{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Apt where

import Control.Monad
import Control.Monad.Trans
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Params as P
import Debian.Changes (ChangeLogEntry(logVersion))
import Debian.Repo
import Debian.Sources
import Debian.Version
import System.Unix.Directory

-- | A package retrieved via apt-get in the given slice
data Apt = Apt NamedSliceList String (Maybe DebianVersion) DebianBuildTree

instance Show Apt where
    show (Apt d n v _) = "apt:" ++ (sliceName . sliceListName $ d) ++ ":" ++ n ++ maybe "" (("=" ++) . show) v

documentation = [ "apt:<distribution>:<packagename> - a target of this form looks up"
                , "the sources.list named <distribution> and retrieves the package with"
                , "the given name from that distribution." ]

-- | Apt targets have no revision string, they just have a version
-- number.  This means that we don't have to build version 0.5.12
-- of a package if it is already in the apt repository.
instance BuildTarget Apt where
    getTop _ (Apt _ _ _ t) = topdir t
    revision _ (Apt d p (Just v) _) = return $ "apt:" ++ (sliceName . sliceListName $ d) ++ ":" ++ p ++ "=" ++ show v
    revision _ (Apt _ _ Nothing _) = fail "Attempt to generate revision string for unversioned apt package"
    logText (Apt name _ _ _) revision = "Built from " ++ sliceName (sliceListName name) ++ " apt pool, apt-revision: " ++ either show id revision

prepare :: P.CacheRec -> String -> String -> Maybe String -> AptIOT IO Apt
prepare cache dist package version =
    do let distro = maybe (error $ "Invalid dist: " ++ sliceName dist') id (findRelease (P.allSources cache) dist')
       os <- prepareAptEnv (P.topDir cache) (P.ifSourcesChanged (P.params cache)) distro
       --when flush (lift $ removeRecursiveSafely $ ReleaseCache.aptDir distro package)
       when (P.flushSource (P.params cache)) (liftIO . removeRecursiveSafely $ aptDir os package)
       tree <- lift $ Debian.Repo.aptGetSource (aptDir os package) os package version'
       let version'' = logVersion . entry $ tree
       return $ Apt distro package (Just version'') tree
    where
      dist' = SliceName dist
      version' = fmap parseDebianVersion version
      findRelease distros dist = 
          case filter ((== dist) . sliceListName) distros of
            [a] -> Just a
            [] -> Nothing
            a -> error $ ("Multiple sources.lists found for " ++ sliceName dist ++ ": " ++ show (map (sliceName . sliceListName) a))
