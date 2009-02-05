module Debian.AutoBuilder.ParamRec
    ( ParamRec(..)
    , makeParamRec
    , CacheRec(..)
    ) where

import qualified Debian.GenBuildDeps as G
import Debian.Repo
import Debian.URI
import Debian.Version
import Debian.AutoBuilder.ParamClass as P

data ParamRec =
    ParamRec
    { 
    -- Global parameters
      verbosity :: Int
    , topDirParam :: Maybe FilePath
    , debug :: Bool
    , dryRun :: Bool
    , requiredVersion :: [(DebianVersion, Maybe String)]
    , showSources :: Bool
    , showParams :: Bool
    , flushAll :: Bool
    , useRepoCache :: Bool
    -- Obtaining and Preparing Source
    , sources :: [(String, String)]
    , targets :: [String]
    , goals :: [String]
    , omitTargets :: [String]
    , vendorTag :: String
    , extraReleaseTag :: Maybe Int
    , flushSource :: Bool
    -- Build Environment
    , forceBuild :: [String]
    , allowBuildDependencyRegressions :: Bool
    , preferred :: [String]
    , strictness :: Strictness
    , setEnv :: [String]
    , buildDepends :: [String]
    , relaxDepends :: G.RelaxInfo
    , noClean :: Bool
    , extraPackages :: [String]
    , extraEssential :: [String]
    , omitEssential :: [String]
    , omitBuildEssential :: Bool
    , baseRelease :: SliceName
    , buildRelease :: ReleaseName
    , doNotChangeVersion :: Bool
    , isDevelopmentRelease :: Bool
    , releaseAliases :: [(String, String)]
    , flushRoot :: Bool
    -- Local Repository
    , cleanUp :: Bool
    , archList :: [Arch]
    , flushPool :: Bool
    -- Uploading
    , doUpload :: Bool
    , doNewDist :: Bool
    , newDistProgram :: String
    , uploadHost :: Maybe String
    , uploadURI :: Maybe URI
    , buildURI :: Maybe URI
    , createRelease :: [String]
    , ifSourcesChanged :: SourcesChangedAction
    , doSSHExport :: Bool
    , autobuilderEmail :: String
    } deriving Show 

instance Show G.RelaxInfo where
    show (G.RelaxInfo pairs) = "RelaxInfo [" ++ show pairs ++ "]"

instance Show SliceName where
    show x = "SliceName { sliceName = " ++ show (sliceName x) ++ " }"

data CacheRec
    = CacheRec'
    { topDir :: FilePath
    , allSources :: [NamedSliceList]
    , buildRepoSources :: SliceList
    }

instance Show SourcesChangedAction where
    show SourcesChangedError = "SourcesChangedError"
    show UpdateSources = "UpdateSources"
    show RemoveRelease = "RemoveRelease"

instance ParamClass ParamRec where
    verbosity = Debian.AutoBuilder.ParamRec.verbosity
    topDirParam = Debian.AutoBuilder.ParamRec.topDirParam
    debug = Debian.AutoBuilder.ParamRec.debug
    dryRun = Debian.AutoBuilder.ParamRec.dryRun
    requiredVersion = Debian.AutoBuilder.ParamRec.requiredVersion
    showSources = Debian.AutoBuilder.ParamRec.showSources
    showParams = Debian.AutoBuilder.ParamRec.showParams
    flushAll = Debian.AutoBuilder.ParamRec.flushAll
    useRepoCache = Debian.AutoBuilder.ParamRec.useRepoCache
    sources = Debian.AutoBuilder.ParamRec.sources
    targets = Debian.AutoBuilder.ParamRec.targets
    goals = Debian.AutoBuilder.ParamRec.goals
    omitTargets = Debian.AutoBuilder.ParamRec.omitTargets
    vendorTag = Debian.AutoBuilder.ParamRec.vendorTag
    extraReleaseTag = Debian.AutoBuilder.ParamRec.extraReleaseTag
    flushSource = Debian.AutoBuilder.ParamRec.flushSource
    forceBuild = Debian.AutoBuilder.ParamRec.forceBuild
    allowBuildDependencyRegressions = Debian.AutoBuilder.ParamRec.allowBuildDependencyRegressions
    preferred = Debian.AutoBuilder.ParamRec.preferred
    strictness = Debian.AutoBuilder.ParamRec.strictness
    setEnv = Debian.AutoBuilder.ParamRec.setEnv
    buildDepends = Debian.AutoBuilder.ParamRec.buildDepends
    relaxDepends = Debian.AutoBuilder.ParamRec.relaxDepends
    noClean = Debian.AutoBuilder.ParamRec.noClean
    extraPackages = Debian.AutoBuilder.ParamRec.extraPackages
    extraEssential = Debian.AutoBuilder.ParamRec.extraEssential
    omitEssential = Debian.AutoBuilder.ParamRec.omitEssential
    omitBuildEssential = Debian.AutoBuilder.ParamRec.omitBuildEssential
    baseRelease = Debian.AutoBuilder.ParamRec.baseRelease
    buildRelease = Debian.AutoBuilder.ParamRec.buildRelease
    doNotChangeVersion = Debian.AutoBuilder.ParamRec.doNotChangeVersion
    isDevelopmentRelease = Debian.AutoBuilder.ParamRec.isDevelopmentRelease
    releaseAliases = Debian.AutoBuilder.ParamRec.releaseAliases
    flushRoot = Debian.AutoBuilder.ParamRec.flushRoot
    cleanUp = Debian.AutoBuilder.ParamRec.cleanUp
    archList = Debian.AutoBuilder.ParamRec.archList
    flushPool = Debian.AutoBuilder.ParamRec.flushPool
    doUpload = Debian.AutoBuilder.ParamRec.doUpload
    doNewDist = Debian.AutoBuilder.ParamRec.doNewDist
    newDistProgram = Debian.AutoBuilder.ParamRec.newDistProgram
    uploadHost = Debian.AutoBuilder.ParamRec.uploadHost
    uploadURI = Debian.AutoBuilder.ParamRec.uploadURI
    buildURI = Debian.AutoBuilder.ParamRec.buildURI
    createRelease = Debian.AutoBuilder.ParamRec.createRelease
    ifSourcesChanged = Debian.AutoBuilder.ParamRec.ifSourcesChanged
    doSSHExport = Debian.AutoBuilder.ParamRec.doSSHExport
    autobuilderEmail = Debian.AutoBuilder.ParamRec.autobuilderEmail

makeParamRec :: ParamClass p => p -> ParamRec
makeParamRec params =
    ParamRec
    { Debian.AutoBuilder.ParamRec.verbosity = P.verbosity params
    , Debian.AutoBuilder.ParamRec.topDirParam = P.topDirParam params
    , Debian.AutoBuilder.ParamRec.debug = P.debug params
    , Debian.AutoBuilder.ParamRec.dryRun = P.dryRun params
    , Debian.AutoBuilder.ParamRec.requiredVersion = P.requiredVersion params
    , Debian.AutoBuilder.ParamRec.showSources = P.showSources params
    , Debian.AutoBuilder.ParamRec.showParams = P.showParams params
    , Debian.AutoBuilder.ParamRec.flushAll = P.flushAll params
    , Debian.AutoBuilder.ParamRec.useRepoCache = P.useRepoCache params
    , Debian.AutoBuilder.ParamRec.sources = P.sources params
    , Debian.AutoBuilder.ParamRec.targets = P.targets params
    , Debian.AutoBuilder.ParamRec.goals = P.goals params
    , Debian.AutoBuilder.ParamRec.omitTargets = P.omitTargets params
    , Debian.AutoBuilder.ParamRec.vendorTag = P.vendorTag params
    , Debian.AutoBuilder.ParamRec.extraReleaseTag = P.extraReleaseTag params
    , Debian.AutoBuilder.ParamRec.flushSource = P.flushSource params
    , Debian.AutoBuilder.ParamRec.forceBuild = P.forceBuild params
    , Debian.AutoBuilder.ParamRec.allowBuildDependencyRegressions = P.allowBuildDependencyRegressions params
    , Debian.AutoBuilder.ParamRec.preferred = P.preferred params
    , Debian.AutoBuilder.ParamRec.strictness = P.strictness params
    , Debian.AutoBuilder.ParamRec.setEnv = P.setEnv params
    , Debian.AutoBuilder.ParamRec.buildDepends = P.buildDepends params
    , Debian.AutoBuilder.ParamRec.relaxDepends = P.relaxDepends params
    , Debian.AutoBuilder.ParamRec.noClean = P.noClean params
    , Debian.AutoBuilder.ParamRec.extraPackages = P.extraPackages params
    , Debian.AutoBuilder.ParamRec.extraEssential = P.extraEssential params
    , Debian.AutoBuilder.ParamRec.omitEssential = P.omitEssential params
    , Debian.AutoBuilder.ParamRec.omitBuildEssential = P.omitBuildEssential params
    , Debian.AutoBuilder.ParamRec.baseRelease = P.baseRelease params
    , Debian.AutoBuilder.ParamRec.buildRelease = P.buildRelease params
    , Debian.AutoBuilder.ParamRec.doNotChangeVersion = P.doNotChangeVersion params
    , Debian.AutoBuilder.ParamRec.isDevelopmentRelease = P.isDevelopmentRelease params
    , Debian.AutoBuilder.ParamRec.releaseAliases = P.releaseAliases params
    , Debian.AutoBuilder.ParamRec.flushRoot = P.flushRoot params
    , Debian.AutoBuilder.ParamRec.cleanUp = P.cleanUp params
    , Debian.AutoBuilder.ParamRec.archList = P.archList params
    , Debian.AutoBuilder.ParamRec.flushPool = P.flushPool params
    , Debian.AutoBuilder.ParamRec.doUpload = P.doUpload params
    , Debian.AutoBuilder.ParamRec.doNewDist = P.doNewDist params
    , Debian.AutoBuilder.ParamRec.newDistProgram = P.newDistProgram params
    , Debian.AutoBuilder.ParamRec.uploadHost = P.uploadHost params
    , Debian.AutoBuilder.ParamRec.uploadURI = P.uploadURI params
    , Debian.AutoBuilder.ParamRec.buildURI = P.buildURI params
    , Debian.AutoBuilder.ParamRec.createRelease = P.createRelease params
    , Debian.AutoBuilder.ParamRec.ifSourcesChanged = P.ifSourcesChanged params
    , Debian.AutoBuilder.ParamRec.doSSHExport = P.doSSHExport params
    , Debian.AutoBuilder.ParamRec.autobuilderEmail = P.autobuilderEmail params
    }
