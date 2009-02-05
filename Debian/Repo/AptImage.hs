-- |The AptImage object represents a partial OS image which is capable
-- of running apt-get, and thus obtaining repository info and source
-- code packages.
module Debian.Repo.AptImage 
    ( prepareAptEnv
    , updateAptEnv
    , aptGetSource
    ) where

import		 Debian.Shell
import		 Debian.Repo.Cache
import		 Debian.Repo.Changes
import		 Debian.Repo.Package
import		 Debian.Repo.IO
import		 Debian.Repo.Slice
import		 Debian.Repo.SourceTree
import		 Debian.Repo.Types
import		 Debian.Relation
import		 Debian.Version

import		 Control.Exception
import		 Control.Monad.State (get, put)
import		 Control.Monad.Trans
import		 Extra.CIO
import		 Data.List
import		 Data.Maybe
import		 Extra.Files
import		 Extra.List
import		 System.Unix.Directory
import		 System.Unix.Process
import		 System.Cmd
import		 System.Directory

instance Show AptImage where
    show apt = "AptImage " ++ relName (aptImageReleaseName apt)

instance AptCache AptImage where
    globalCacheDir = aptGlobalCacheDir
    rootDir = aptImageRoot
    aptArch = aptImageArch
    aptBaseSliceList = aptImageSliceList
    aptSourcePackages = aptImageSourcePackages
    aptBinaryPackages = aptImageBinaryPackages
    aptReleaseName = aptImageReleaseName

instance Ord AptImage where
    compare a b = compare (aptImageReleaseName a) (aptImageReleaseName b)

instance Eq AptImage where
    a == b = compare a b == EQ

prepareAptEnv :: CIO m
	      => FilePath		-- Put environment in a subdirectory of this
              -> SourcesChangedAction	-- What to do if environment already exists and sources.list is different
              -> NamedSliceList		-- The sources.list
              -> AptIOT m AptImage		-- The resulting environment
prepareAptEnv cacheDir sourcesChangedAction sources =
    get >>= return . lookupAptImage (sliceListName sources) >>=
    maybe (prepareAptEnv' cacheDir sourcesChangedAction sources) return

-- |Create a skeletal enviroment sufficient to run apt-get.
{-# NOINLINE prepareAptEnv' #-}
prepareAptEnv' :: CIO m => FilePath -> SourcesChangedAction -> NamedSliceList -> AptIOT m AptImage
prepareAptEnv' cacheDir sourcesChangedAction sources =
    do let root = rootPath (cacheRootDir cacheDir (ReleaseName (sliceName (sliceListName sources))))
       --vPutStrLn 2 $ "prepareAptEnv " ++ sliceName (sliceListName sources)
       io $ createDirectoryIfMissing True (root ++ "/var/lib/apt/lists/partial")
       io $ createDirectoryIfMissing True (root ++ "/var/lib/apt/lists/partial")
       io $ createDirectoryIfMissing True (root ++ "/var/cache/apt/archives/partial")
       io $ createDirectoryIfMissing True (root ++ "/var/lib/dpkg")
       io $ createDirectoryIfMissing True (root ++ "/etc/apt")
       io $ writeFileIfMissing True (root ++ "/var/lib/dpkg/status") ""
       io $ writeFileIfMissing True (root ++ "/var/lib/dpkg/diversions") ""
       -- We need to create the local pool before updating so the
       -- sources.list will be valid.
       let sourceListText = show (sliceList sources)
       -- ePut ("writeFile " ++ (root ++ "/etc/apt/sources.list") ++ "\n" ++ sourceListText)
       io $ replaceFile (root ++ "/etc/apt/sources.list") sourceListText
       arch <- io $ buildArchOfRoot
       let os = AptImage { aptGlobalCacheDir = cacheDir
                         , aptImageRoot = EnvRoot root
                         , aptImageArch = arch
                         , aptImageReleaseName = ReleaseName . sliceName . sliceListName $ sources
                         , aptImageSliceList = sliceList sources
                         , aptImageSourcePackages = []
                         , aptImageBinaryPackages = [] }
       os' <- updateCacheSources sourcesChangedAction os >>= updateAptEnv
       get >>= put . insertAptImage (sliceListName sources) os'
       return os'

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
{-# NOINLINE updateAptEnv #-}
updateAptEnv :: CIO m => AptImage -> AptIOT m AptImage
updateAptEnv os =
    do
      io $ system ("apt-get" ++ aptOpts os ++ " update >/dev/null 2>&1")
      -- Make the loading of the package indexes lazy.
      sourcePackages <- getSourcePackages os >>= return . sortBy cmp
      binaryPackages <- getBinaryPackages os
      return $ os { aptImageSourcePackages = sourcePackages
                  , aptImageBinaryPackages = binaryPackages }
    where
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

getSourcePackages :: CIO m => AptImage -> AptIOT m [SourcePackage]
getSourcePackages os = 
    mapM (sourcePackagesOfIndex' os) indexes >>= return . concat
    where
      indexes = concat . map (sliceIndexes os) . slices . sourceSlices . aptImageSliceList $ os

getBinaryPackages :: CIO m => AptImage -> AptIOT m [BinaryPackage]
getBinaryPackages os = 
    mapM (binaryPackagesOfIndex' os) indexes >>= return . concat
    where
      indexes = concat . map (sliceIndexes os) . slices . binarySlices . aptImageSliceList $ os

-- |Retrieve a source package via apt-get.
aptGetSource :: (AptCache t, CIO m)
             => FilePath			-- Where to put the source
             -> t			-- Where to apt-get from
             -> PkgName			-- The name of the package
             -> Maybe DebianVersion	-- The desired version, if Nothing get newest
             -> m DebianBuildTree	-- The resulting source tree
aptGetSource dir os package version =
    do liftIO $ createDirectoryIfMissing True dir
       ready <- findDebianBuildTrees dir
       let newest = listToMaybe . map (packageVersion . sourcePackageID) . filter ((== package) . packageName . sourcePackageID) . aptSourcePackages $ os
       let version' = maybe newest Just version
       case (version', ready) of
         (Nothing, _) ->
             error ("No available versions of " ++ package ++ " in " ++ rootPath (rootDir os))
         (Just requested, [tree])
             | requested == (logVersion . entry $ tree) ->
                 return tree
         (Just requested, []) ->
             do runAptGet os dir "source" [(package, Just requested)]
                trees <- findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> error "apt-get source failed"
         (Just requested, _) ->
             do -- One or more incorrect versions are available, remove them
                liftIO $ removeRecursiveSafely dir
                vBOL 0
                vEPutStr 0 $ "Retrieving APT source for " ++ package
                runAptGet os dir "source" [(package, Just requested)]
                trees <- findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> error "apt-get source failed"
{-
    where
      _availableStyle = setStyle (setStart (Just ("Finding available versions of " ++ package ++ " in APT pool")))
      aptGetStyle = setStyle (setStart (Just ("Retrieving APT source for " ++ package)))
-}

runAptGet :: (AptCache t, CIO m) => t -> FilePath -> String -> [(PkgName, Maybe DebianVersion)] -> m (Either String [Output])
runAptGet os dir command packages =
    mkdir >>= aptget
    where
      mkdir = liftIO . try . createDirectoryIfMissing True $ dir
      aptget (Left e) = return . Left . show $ e
      aptget (Right _) = runTaskAndTest (SimpleTask 0 cmd)          
      cmd = (consperse " " ("cd" : dir : "&&" : "apt-get" : aptOpts os : command : map formatPackage packages))
      formatPackage (name, Nothing) = name
      formatPackage (name, Just version) = name ++ "=" ++ show version

aptOpts :: AptCache t => t -> String
aptOpts os =
    (" -o=Dir::State::status=" ++ root ++ "/var/lib/dpkg/status" ++
     " -o=Dir::State::Lists=" ++ root ++ "/var/lib/apt/lists" ++
     " -o=Dir::Cache::Archives=" ++ root ++ "/var/cache/apt/archives" ++
     " -o=Dir::Etc::SourceList=" ++ root ++ "/etc/apt/sources.list")
    where root = rootPath . rootDir $ os
