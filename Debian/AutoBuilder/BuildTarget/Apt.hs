module Debian.AutoBuilder.BuildTarget.Apt where

import Debian.Repo
import Debian.Version

import Debian.AutoBuilder.BuildTarget
import Debian.AutoBuilder.ParamClass (ParamClass)
import qualified Debian.AutoBuilder.ParamClass as P
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import System.Unix.Directory
import Text.Regex
import Extra.CIO

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
    revision _ (Apt d p (Just v) _) = return (Right $ "apt:" ++ (sliceName . sliceListName $ d) ++ ":" ++ p ++ "=" ++ show v)
    revision _ (Apt _ _ Nothing _) = error "Attempt to generate revision string for unversioned apt package"
    logText (Apt name _ _ _) _ = "Built from " ++ sliceName (sliceListName name) ++ " apt pool"

prepareApt :: (ParamClass p, CIO m) => p -> String -> AptIOT m (Either String Tgt)
prepareApt params target =
    do
      let (dist, package, version) =
              case ms of
                Just [release,package,_,""] -> (SliceName release, package, Nothing)
                Just [release,package,_,version] -> (SliceName release, package, (Just $ parseDebianVersion version))
                _ -> error ("failed parsing apt target: (expected dist:package[:version]): " ++ target)
      let distro = maybe (error $ "Invalid dist: " ++ sliceName dist) id (findRelease (P.allSources params) dist)
      os <- prepareAptEnv (P.topDir params) (P.ifSourcesChanged params) distro
      --when flush (lift $ removeRecursiveSafely $ ReleaseCache.aptDir distro package)
      when (P.flushSource params) (liftIO . removeRecursiveSafely $ aptDir os package)
      tree <- lift $ Debian.Repo.aptGetSource (aptDir os package) os package version
      let version' = logVersion . entry $ tree
      return . Right . Tgt $ Apt distro package (Just version') tree
    where
      ms = match "([^:]+):([^=]*)(=([^ \t\n]+))?" target
      match = matchRegex . mkRegex
      findRelease distros dist = 
          case filter ((== dist) . sliceListName) distros of
            [a] -> Just a
            [] -> Nothing
            a -> error $ ("Multiple sources.lists found for " ++ sliceName dist ++ ": " ++ show (map (sliceName . sliceListName) a))
