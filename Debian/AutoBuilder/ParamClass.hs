module Debian.AutoBuilder.ParamClass
    ( topDirDefault
    , ParamClass(..)
    , CacheClass(..)
    , Cache(..)
    , buildCache
    , RunClass
    , Strictness(Strict, Moderate, Lax)
    , Target(..)
    , prettyPrint
    , findSlice
    , dirtyRootOfRelease
    , cleanRootOfRelease
    , dirtyRoot
    , cleanRoot
    , localPoolDir
    ) where

import           Control.Exception (try, evaluate)
import           Control.Monad.State (get, put)
import		 Control.Monad.Trans (lift, liftIO)
import		 Data.Maybe
import           Data.Map (fromList)
import		 Debian.Repo.Cache (SourcesChangedAction)
import           Debian.Repo.IO (AptIOT)
import           Debian.Repo(EnvRoot(EnvRoot), Arch, SliceName(..),
                             SliceList(..), NamedSliceList(..), ReleaseName, releaseName',
                             setRepoMap, parseSourcesList, verifySourcesList, repoSources)
import		 Debian.Version
import		 Debian.URI
import qualified Debian.GenBuildDeps as G
import           Extra.CIO (CIO, ePutStrBl)
import		 System.Directory (createDirectoryIfMissing, getPermissions, writable)
import		 System.Environment (getEnv)
import qualified System.IO as IO

-- Lax: dependencies are installed into clean, clean synced to build for each target
-- Moderate: dependencies are installed into build, clean synced to build only at beginning of run
-- Strict: dependencies are installed into build, clean synced to build for each target
data Strictness
    = Lax |		-- Let build dependencies accumulate
      Moderate |	-- Install only required build dependencies
      Strict		-- Create a new build environment for each package
      deriving Eq

instance Show Strictness where
    show Lax = "Lax"
    show Moderate = "Moderate"
    show Strict = "Strict"

data Target
    = Target
      { sourcePackageName :: String
      , sourceSpec :: String
      , relaxInfo :: [String]		-- ^ Build dependencies which be ignored when deciding whether to rebuild
      } deriving Show

topDirDefault = "/var/cache/autobuilder"

class ParamClass a where
    -- Global Parameters
    verbosity :: a -> Int
    topDirParam :: a -> Maybe FilePath
    debug :: a -> Bool
    dryRun :: a -> Bool
    requiredVersion :: a -> [(DebianVersion, Maybe String)]
    showSources :: a -> Bool
    showParams :: a -> Bool
    flushAll :: a -> Bool
    useRepoCache :: a -> Bool
    -- Obtaining and Preparing Source
    sources :: a -> [(String, String)]
    targets :: a -> [Target]
    goals :: a -> [String]
    omitTargets :: a -> [String]
    vendorTag :: a -> String
    extraReleaseTag :: a -> Maybe Int
    flushSource :: a -> Bool
    -- Build Environment
    forceBuild :: a -> [String]
    allowBuildDependencyRegressions :: a -> Bool
    preferred :: a -> [String]
    strictness :: a -> Strictness
    setEnv :: a -> [String]
    buildDepends :: a -> [String]
    relaxDepends :: a -> G.RelaxInfo
    noClean :: a -> Bool
    extraPackages :: a -> [String]
    extraEssential :: a -> [String]
    omitEssential :: a -> [String]
    omitBuildEssential :: a -> Bool
    baseRelease :: a -> SliceName
    buildRelease :: a -> ReleaseName
    doNotChangeVersion :: a -> Bool
    isDevelopmentRelease :: a -> Bool
    releaseAliases :: a -> [(String, String)]
    flushRoot :: a -> Bool
    -- Local Repository
    cleanUp :: a -> Bool
    archList :: a -> [Arch]
    flushPool :: a -> Bool
    -- Uploading
    doUpload :: a -> Bool
    doNewDist :: a -> Bool
    newDistProgram :: a -> String
    uploadHost :: a -> Maybe String
    uploadURI :: a -> Maybe URI
    buildURI :: a -> Maybe URI
    createRelease :: a -> [String]
    ifSourcesChanged :: a -> SourcesChangedAction
    doSSHExport :: a -> Bool
    autobuilderEmail :: a -> String

instance ParamClass p => ParamClass (p, a) where
    verbosity = verbosity . fst
    topDirParam = topDirParam . fst
    debug = debug . fst
    dryRun = dryRun . fst
    requiredVersion = requiredVersion . fst
    showSources = showSources . fst
    showParams = showParams . fst
    flushAll = flushAll . fst
    useRepoCache = useRepoCache . fst
    sources = sources . fst
    targets = targets . fst
    goals = goals . fst
    omitTargets = omitTargets . fst
    vendorTag = vendorTag . fst
    extraReleaseTag = extraReleaseTag . fst
    flushSource = flushSource . fst
    forceBuild = forceBuild . fst
    allowBuildDependencyRegressions = allowBuildDependencyRegressions . fst
    preferred = preferred . fst
    strictness = strictness . fst
    setEnv = setEnv . fst
    buildDepends = buildDepends . fst
    relaxDepends = relaxDepends . fst
    noClean = noClean . fst
    extraPackages = extraPackages . fst
    extraEssential = extraEssential . fst
    omitEssential = omitEssential . fst
    omitBuildEssential = omitBuildEssential . fst
    baseRelease = baseRelease . fst
    buildRelease = buildRelease . fst
    doNotChangeVersion = doNotChangeVersion . fst
    isDevelopmentRelease = isDevelopmentRelease . fst
    releaseAliases = releaseAliases . fst
    flushRoot = flushRoot . fst
    cleanUp = cleanUp . fst
    archList = archList . fst
    flushPool = flushPool . fst
    doUpload = doUpload . fst
    doNewDist = doNewDist . fst
    newDistProgram = newDistProgram . fst
    uploadHost = uploadHost . fst
    uploadURI = uploadURI . fst
    buildURI = buildURI . fst
    createRelease = createRelease . fst
    ifSourcesChanged = ifSourcesChanged . fst
    doSSHExport = doSSHExport . fst
    autobuilderEmail = autobuilderEmail . fst

prettyPrint :: ParamClass p => p -> String
prettyPrint x =
    unlines [ "verbosity=" ++ take 120 (show (verbosity x))
            , "topDirParam=" ++ take 120 (show (topDirParam x))
            , "debug=" ++ take 120 (show (debug x))
            , "dryRun=" ++ take 120 (show (dryRun x))
            , "requiredVersion=" ++ take 120 (show (requiredVersion x))
            , "showSources=" ++ take 120 (show (showSources x))
            , "showParams=" ++ take 120 (show (showParams x))
            , "flushAll=" ++ take 120 (show (flushAll x))
            , "useRepoCache=" ++ take 120 (show (useRepoCache x))
            , "sources=" ++ take 120 (show (sources x))
            , "targets=" ++ take 120 (show (map sourcePackageName (targets x)))
            , "goals=" ++ take 120 (show (goals x))
            , "omitTargets=" ++ take 120 (show (omitTargets x))
            , "vendorTag=" ++ take 120 (show (vendorTag x))
            , "extraReleaseTag=" ++ take 120 (show (extraReleaseTag x))
            , "flushSource=" ++ take 120 (show (flushSource x))
            , "forceBuild=" ++ take 120 (show (forceBuild x))
            , "allowBuildDependencyRegressions=" ++ take 120 (show (allowBuildDependencyRegressions x))
            , "preferred=" ++ take 120 (show (preferred x))
            , "strictness=" ++ take 120 (show (strictness x))
            , "setEnv=" ++ take 120 (show (setEnv x))
            , "buildDepends=" ++ take 120 (show (buildDepends x))
            --, "relaxDepends=" ++ take 120 (show (relaxDepends x))
            , "noClean=" ++ take 120 (show (noClean x))
            , "extraPackages=" ++ take 120 (show (extraPackages x))
            , "extraEssential=" ++ take 120 (show (extraEssential x))
            , "omitEssential=" ++ take 120 (show (omitEssential x))
            , "omitBuildEssential=" ++ take 120 (show (omitBuildEssential x))
            , "baseRelease=" ++ take 120 (show (sliceName (baseRelease x)))
            , "buildRelease=" ++ take 120 (show (buildRelease x))
            , "doNotChangeVersion=" ++ take 120 (show (doNotChangeVersion x))
            , "isDevelopmentRelease=" ++ take 120 (show (isDevelopmentRelease x))
            , "releaseAliases=" ++ take 120 (show (releaseAliases x))
            , "flushRoot=" ++ take 120 (show (flushRoot x))
            , "cleanUp=" ++ take 120 (show (cleanUp x))
            , "archList=" ++ take 120 (show (archList x))
            , "flushPool=" ++ take 120 (show (flushPool x))
            , "doUpload=" ++ take 120 (show (doUpload x))
            , "doNewDist=" ++ take 120 (show (doNewDist x))
            , "newDistProgram=" ++ take 120 (show (newDistProgram x))
            , "uploadHost=" ++ take 120 (show (uploadHost x))
            , "uploadURI=" ++ take 120 (show (uploadURI x))
            , "buildURI=" ++ take 120 (show (buildURI x))
            , "createRelease=" ++ take 120 (show (createRelease x))
            --, "ifSourcesChanged=" ++ take 120 (show (ifSourcesChanged x))
            , "doSSHExport=" ++ take 120 (show (doSSHExport x))
            , "autobuilderEmail=" ++ take 120 (show (autobuilderEmail x))
            , "baseRelease sources=\n" ++ show (lookup (sliceName (baseRelease x)) (sources x))
            ]

class CacheClass a where
    topDir :: a -> FilePath
    allSources :: a -> [NamedSliceList]
    buildRepoSources :: a -> SliceList

instance CacheClass c => CacheClass (a, c) where
    topDir = topDir . snd
    allSources = allSources . snd
    buildRepoSources = buildRepoSources . snd

data Cache
    = Cache { topDir' :: FilePath
            , allSources' :: [NamedSliceList]
            , buildRepoSources' :: SliceList
            }

instance CacheClass Cache where
    topDir = topDir'
    allSources = allSources'
    buildRepoSources = buildRepoSources'

buildCache :: (ParamClass p, CIO m) => p -> AptIOT m Cache
buildCache params =
    do top <- lift $ computeTopDir params
       loadRepoCache top
       all <- mapM parseNamedSliceList (sources params)
       let uri = maybe (uploadURI params) Just (buildURI params)
       build <- maybe (return $ SliceList { slices = [] }) (repoSources Nothing) uri
       return $ Cache {topDir' = top, allSources' = all, buildRepoSources' = build}
    where
      parseNamedSliceList (name, text) = 
          do sources <- (verifySourcesList Nothing . parseSourcesList) text
             return $ NamedSliceList { sliceListName = SliceName name, sliceList = sources }

loadRepoCache :: CIO m => FilePath -> AptIOT m ()
loadRepoCache top =
    do lift $ ePutStrBl "Loading repo cache..."
       state <- get
       uris <- liftIO $ try (readFile (top ++ "/repoCache")) >>= try . evaluate . either (const []) read >>= return . either (const []) id
       put (setRepoMap (fromList (map fixURI uris)) state)
    where
      fixURI (s, x) = (fromJust (parseURI s), x)

-- Compute the top directory, try to create it, and then make sure it
-- exists.  Then we can safely return it from topDir below.
computeTopDir :: (ParamClass p, CIO m) => p -> m FilePath
computeTopDir params =
    do top <- maybe homeDir return (topDirParam params)
       liftIO (try $ createDirectoryIfMissing True top)
       result <- liftIO (try $ getPermissions top >>= return . writable)
       case result of
         Left _ -> error $ "Could not create cache directory " ++ top ++ " (are you root?)"
         Right False -> error "Cache directory not writable (are you root?)"
         Right True -> return top
    where
      homeDir = liftIO (try (getEnv "HOME")) >>= return . either (const topDirDefault) (++ "/.autobuilder")

class (ParamClass a, CacheClass a) => RunClass a

-- |Find a release by name, among all the "Sources" entries given in the configuration.
findSlice :: CacheClass c => c -> SliceName -> Either String NamedSliceList
findSlice cache dist =
    case filter ((== dist) . sliceListName) (allSources cache) of
      [x] -> Right x
      [] -> Left ("No sources.list found for " ++ sliceName dist)
      xs -> Left ("Multiple sources.lists found for " ++ sliceName dist ++ "\n" ++ show (map (sliceName . sliceListName) xs))

dirtyRootOfRelease :: (CacheClass a, ParamClass a) => a -> ReleaseName -> EnvRoot
dirtyRootOfRelease params distro =
    EnvRoot $ topDir params ++ "/dists/" ++ releaseName' distro ++ "/build-" ++ (show (strictness params))
    --ReleaseCache.dirtyRoot distro (show (strictness params))

cleanRootOfRelease :: (CacheClass a, ParamClass a) => a -> ReleaseName -> EnvRoot
cleanRootOfRelease params distro =
    EnvRoot $ topDir params ++ "/dists/" ++ releaseName' distro ++ "/clean-" ++ (show (strictness params))
    --ReleaseCache.cleanRoot distro (show (strictness params))

dirtyRoot :: (CacheClass a, ParamClass a) => a -> EnvRoot
dirtyRoot params = dirtyRootOfRelease params (buildRelease params)
    --EnvRoot $ topDir params ++ "/dists/" ++ show (buildRelease params) ++ "/build-" ++ (show (strictness params))

cleanRoot :: (CacheClass a, ParamClass a) => a -> EnvRoot
cleanRoot params = cleanRootOfRelease params (buildRelease params)
    -- cleanRootOfRelease params (buildRelease params)

-- |Location of the local repository for uploaded packages.
localPoolDir :: (CacheClass a, ParamClass a) => a -> FilePath
localPoolDir params = topDir params ++ "/localpools/" ++ releaseName' (buildRelease params)
