module Debian.AutoBuilder.ParamRec
    ( ParamRec(..)
    , TargetSpec(..)
    , adjustVendorTag -- Export for testing
    ) where

import qualified Data.Set as Set
import Debian.Repo
import Debian.URI
import Debian.Version
import Debian.AutoBuilder.ParamClass as P

data ParamRec =
    ParamRec
    { verbosity :: Int
    , topDirParam :: Maybe FilePath
    , debug :: Bool
    , dryRun :: Bool
    , requiredVersion :: [(DebianVersion, Maybe String)]
    , showSources :: Bool
    , showParams :: Bool
    , flushAll :: Bool
    , useRepoCache :: Bool
    , sources :: [(String, String)]
    , targets :: TargetSpec
    , goals :: [String]
    , omitTargets :: [String]
    , vendorTag :: String
    , oldVendorTags :: [String]
    , extraReleaseTag :: Maybe Int
    , flushSource :: Bool
    , forceBuild :: [String]
    , buildTrumped :: [String]
    , allowBuildDependencyRegressions :: Bool
    , preferred :: [String]
    , strictness :: Strictness
    , setEnv :: [String]
    , buildDepends :: [String]
    , globalRelaxInfo :: [String]
    , noClean :: Bool
    , extraPackages :: [String]
    , extraEssential :: [String]
    , omitEssential :: [String]
    , omitBuildEssential :: Bool
    , buildRelease :: ReleaseName
    , releaseSuffixes :: [String]
    , developmentReleaseNames :: [String]
    , doNotChangeVersion :: Bool
    , releaseAliases :: [(String, String)]
    , flushRoot :: Bool
    , cleanUp :: Bool
    , archList :: [Arch]
    , flushPool :: Bool
    , doUpload :: Bool
    , doNewDist :: Bool
    , newDistProgram :: String
    , uploadURI :: Maybe URI
    , buildURI :: Maybe URI
    , createRelease :: [String]
    , ifSourcesChanged :: SourcesChangedAction
    , doSSHExport :: Bool
    , doHelp :: Bool
    , autobuilderEmail :: String
    } deriving Show 

-- |Information about what targets to build are temporarily held in a
-- value of this type.  Once all the command line arguments have been
-- analyzed, this is transformed into a set of targets, which can be
-- used to implement the ParamClass "targets" method.
data TargetSpec
    = AllTargets
    | TargetNames (Set.Set String)
    | TargetSet (Set.Set Target)
    deriving Show

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
    targets p = case Debian.AutoBuilder.ParamRec.targets p of
                  TargetSet xs -> xs
                  x -> error $ "Expected TargetSet, found " ++ show x
    goals = Debian.AutoBuilder.ParamRec.goals
    omitTargets = Debian.AutoBuilder.ParamRec.omitTargets
    vendorTag = adjustVendorTag . Debian.AutoBuilder.ParamRec.vendorTag
    oldVendorTags = Debian.AutoBuilder.ParamRec.oldVendorTags
    extraReleaseTag = Debian.AutoBuilder.ParamRec.extraReleaseTag
    flushSource = Debian.AutoBuilder.ParamRec.flushSource
    forceBuild = Debian.AutoBuilder.ParamRec.forceBuild
    buildTrumped = Debian.AutoBuilder.ParamRec.buildTrumped
    allowBuildDependencyRegressions = Debian.AutoBuilder.ParamRec.allowBuildDependencyRegressions
    preferred = Debian.AutoBuilder.ParamRec.preferred
    strictness = Debian.AutoBuilder.ParamRec.strictness
    setEnv = Debian.AutoBuilder.ParamRec.setEnv
    buildDepends = Debian.AutoBuilder.ParamRec.buildDepends
    globalRelaxInfo = Debian.AutoBuilder.ParamRec.globalRelaxInfo
    noClean = Debian.AutoBuilder.ParamRec.noClean
    extraPackages = Debian.AutoBuilder.ParamRec.extraPackages
    extraEssential = Debian.AutoBuilder.ParamRec.extraEssential
    omitEssential = Debian.AutoBuilder.ParamRec.omitEssential
    omitBuildEssential = Debian.AutoBuilder.ParamRec.omitBuildEssential
    buildRelease = Debian.AutoBuilder.ParamRec.buildRelease
    releaseSuffixes = Debian.AutoBuilder.ParamRec.releaseSuffixes
    developmentReleaseNames = Debian.AutoBuilder.ParamRec.developmentReleaseNames
    doNotChangeVersion = Debian.AutoBuilder.ParamRec.doNotChangeVersion
    releaseAliases = Debian.AutoBuilder.ParamRec.releaseAliases
    flushRoot = Debian.AutoBuilder.ParamRec.flushRoot
    cleanUp = Debian.AutoBuilder.ParamRec.cleanUp
    archList = Debian.AutoBuilder.ParamRec.archList
    flushPool = Debian.AutoBuilder.ParamRec.flushPool
    doUpload = Debian.AutoBuilder.ParamRec.doUpload
    doNewDist = Debian.AutoBuilder.ParamRec.doNewDist
    newDistProgram = Debian.AutoBuilder.ParamRec.newDistProgram
    uploadURI = Debian.AutoBuilder.ParamRec.uploadURI
    buildURI = Debian.AutoBuilder.ParamRec.buildURI
    createRelease = Debian.AutoBuilder.ParamRec.createRelease
    ifSourcesChanged = Debian.AutoBuilder.ParamRec.ifSourcesChanged
    doSSHExport = Debian.AutoBuilder.ParamRec.doSSHExport
    doHelp = Debian.AutoBuilder.ParamRec.doHelp
    autobuilderEmail = Debian.AutoBuilder.ParamRec.autobuilderEmail

-- |Adjust the vendor tag so we don't get trumped by Debian's new +b
-- notion for binary uploads.  The version number of the uploaded
-- binary packages may have "+bNNN" appended, which would cause
-- them to trump the versions constructed by the autobuilder.  So, we
-- prepend a "+" to the vendor string if there isn't one, and if the
-- vendor string starts with the character b or something less, two
-- plus signs are prepended.
adjustVendorTag s =
    newprefix ++ suffix
    where (_oldprefix, suffix) = span (== '+') s
          newprefix = if suffix < "b" then "++" else "+" 
