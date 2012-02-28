{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Apt where

import Control.Monad
import Control.Monad.Trans
import Data.List (sort, nub)
import Data.Maybe (catMaybes)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.BuildTarget.Temp as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.PackageFlag as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.AutoBuilder.Types.RetrieveMethod (RetrieveMethod)
import Debian.Changes (ChangeLogEntry(logVersion))
import Debian.Repo
import Debian.Sources
import Debian.Version
import System.Unix.Directory

-- | A package retrieved via apt-get in the given slice
data Apt = Apt NamedSliceList String (Maybe DebianVersion) DebianBuildTree RetrieveMethod

documentation = [ "apt:<distribution>:<packagename> - a target of this form looks up"
                , "the sources.list named <distribution> and retrieves the package with"
                , "the given name from that distribution." ]

instance Download Apt where
    method (Apt _ _ _ _ m) = m
    getTop _ (Apt _ _ _ t _) = topdir t
    revision _ (Apt d p (Just v) _ _) = return $ "apt:" ++ (sliceName . sliceListName $ d) ++ ":" ++ p ++ "=" ++ show (prettyDebianVersion v)
    revision _ (Apt _ _ Nothing _ _) = fail "Attempt to generate revision string for unversioned apt package"
    logText (Apt name _ _ _ _) revision = "Built from " ++ sliceName (sliceListName name) ++ " apt pool, apt-revision: " ++ either show id revision

-- | Apt targets have no revision string, they just have a version
-- number.  This means that we don't have to build version 0.5.12
-- of a package if it is already in the apt repository.
instance BuildTarget Apt

prepare :: P.CacheRec -> String -> String -> [P.AptFlag] -> RetrieveMethod -> AptIOT IO T.Target
prepare cache dist package flags method =
    do let distro = maybe (error $ "Invalid dist: " ++ sliceName dist') id (findRelease (P.allSources cache) dist')
       os <- prepareAptEnv (P.topDir cache) (P.ifSourcesChanged (P.params cache)) distro
       --when flush (lift $ removeRecursiveSafely $ ReleaseCache.aptDir distro package)
       when (P.flushSource (P.params cache)) (liftIO . removeRecursiveSafely $ aptDir os package)
       tree <- lift $ Debian.Repo.aptGetSource (aptDir os package) os package version'
       let version'' = logVersion . entry $ tree
           rev = "apt:" ++ (sliceName . sliceListName $ distro) ++ ":" ++ package ++ "=" ++ show (prettyDebianVersion version'')
       return $ T.Target
                  { T.download = T.Download 
                                 { T.method' = method
                                 , T.getTop = topdir tree
                                 , T.revision = rev
                                 , T.logText = "Built from " ++ sliceName (sliceListName distro) ++ " apt pool, apt-revision: " ++ rev }
                  , T.mVersion = Nothing
                  , T.origTarball = Nothing
                  , T.debianSourceTree = debTree' tree
                  }
       -- return $ Apt distro package (Just version'') tree method
    where
      dist' = SliceName dist
      version' = case (nub (sort (catMaybes (map (\ flag -> case flag of
                                                   P.AptPin s -> Just (parseDebianVersion s)
                                                   {- _ -> Nothing -}) flags)))) of
                   [] -> Nothing
                   [v] -> Just v
                   vs -> error ("Conflicting pin versions for apt-get: " ++ show (map prettyDebianVersion vs))
      findRelease distros dist = 
          case filter ((== dist) . sliceListName) distros of
            [a] -> Just a
            [] -> Nothing
            a -> error $ ("Multiple sources.lists found for " ++ sliceName dist ++ ": " ++ show (map (sliceName . sliceListName) a))
