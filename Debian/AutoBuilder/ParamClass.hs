module Debian.AutoBuilder.ParamClass
    ( ParamClass(..)
    , Strictness(Strict, Moderate, Lax)
    , findSlice
    , cleanRootOfRelease
    , dirtyRootOfRelease
    , dirtyRoot
    , cleanRoot
    , localPoolDir
    ) where

import		 Data.Maybe
import		 Debian.Repo
import		 Debian.Version
import		 Debian.URI
import qualified Debian.GenBuildDeps as G
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

class ParamClass a where
    prettyPrint :: a -> String
    -- Global Parameters
    _verbosity :: a -> Int
    topDir :: a -> FilePath
    debug :: a -> Bool
    dryRun :: a -> Bool
    requiredVersion :: a -> [(DebianVersion, Maybe String)]
    showSources :: a -> Bool
    showParams :: a -> Bool
    flushAll :: a -> Bool
    useRepoCache :: a -> Bool
    -- Obtaining and Preparing Source
    sources :: a -> [String]
    allSources :: a -> [NamedSliceList]
    buildRepoSources :: a -> SliceList
    targets :: a -> [String]
    goals :: a -> [String]
    omitTargets :: a -> [String]
    vendorTag :: a -> String
    extraReleaseTag :: a -> Maybe Int
    flushSource :: a -> Bool
    -- Build Environment
    forceBuild :: a -> Bool
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
    createRelease :: a -> [String]
    ifSourcesChanged :: a -> SourcesChangedAction
    doSSHExport :: a -> Bool
    autobuilderEmail :: a -> String

-- |Find a release by name, among all the "Sources" entries given in the configuration.
findSlice :: ParamClass p => p -> SliceName -> Either String NamedSliceList
findSlice params dist =
    case filter ((== dist) . sliceListName) (allSources params) of
      [x] -> Right x
      [] -> Left ("No sources.list found for " ++ sliceName dist)
      xs -> Left ("Multiple sources.lists found for " ++ sliceName dist ++ "\n" ++ show (map (sliceName . sliceListName) xs))

dirtyRootOfRelease :: ParamClass p => p -> ReleaseName -> EnvRoot
dirtyRootOfRelease params distro =
    EnvRoot $ topDir params ++ "/dists/" ++ releaseName' distro ++ "/build-" ++ (show (strictness params))
    --ReleaseCache.dirtyRoot distro (show (strictness params))

cleanRootOfRelease :: ParamClass p => p -> ReleaseName -> EnvRoot
cleanRootOfRelease params distro =
    EnvRoot $ topDir params ++ "/dists/" ++ releaseName' distro ++ "/clean-" ++ (show (strictness params))
    --ReleaseCache.cleanRoot distro (show (strictness params))

dirtyRoot :: ParamClass p => p -> EnvRoot
dirtyRoot params = dirtyRootOfRelease params (buildRelease params)
    --EnvRoot $ topDir params ++ "/dists/" ++ show (buildRelease params) ++ "/build-" ++ (show (strictness params))

cleanRoot :: ParamClass p => p -> EnvRoot
cleanRoot params = cleanRootOfRelease params (buildRelease params)
    -- cleanRootOfRelease params (buildRelease params)

-- |Location of the local repository for uploaded packages.
localPoolDir :: ParamClass p => p -> FilePath
localPoolDir params = topDir params ++ "/localpools/" ++ releaseName' (buildRelease params)
