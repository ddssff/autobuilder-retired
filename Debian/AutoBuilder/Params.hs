module Debian.AutoBuilder.Params
    ( CacheRec(..)
    , makeParamRec
    , defaultParams
    ) where

import qualified Debian.AutoBuilder.ParamClass as P
import qualified Debian.AutoBuilder.ParamRec as R
import qualified Debian.GenBuildDeps as G
import		 Debian.Repo.Cache (SourcesChangedAction(SourcesChangedError))
import           Debian.Repo.Types (NamedSliceList, SliceList, Arch(Binary),
                                    SliceName(SliceName, sliceName),
                                    ReleaseName(ReleaseName, relName))
import           Debian.Version (parseDebianVersion)

data CacheRec
    = CacheRec'
    { topDir :: FilePath
    , allSources :: [NamedSliceList]
    , buildRepoSources :: SliceList
    }

makeParamRec :: P.ParamClass p => p -> R.ParamRec
makeParamRec params =
    R.ParamRec
    { R.verbosity = P.verbosity params
    , R.topDirParam = P.topDirParam params
    , R.debug = P.debug params
    , R.dryRun = P.dryRun params
    , R.requiredVersion = P.requiredVersion params
    , R.showSources = P.showSources params
    , R.showParams = P.showParams params
    , R.flushAll = P.flushAll params
    , R.useRepoCache = P.useRepoCache params
    , R.sources = P.sources params
    , R.targets = P.targets params
    , R.goals = P.goals params
    , R.omitTargets = P.omitTargets params
    , R.vendorTag = P.vendorTag params
    , R.extraReleaseTag = P.extraReleaseTag params
    , R.flushSource = P.flushSource params
    , R.forceBuild = P.forceBuild params
    , R.allowBuildDependencyRegressions = P.allowBuildDependencyRegressions params
    , R.preferred = P.preferred params
    , R.strictness = P.strictness params
    , R.setEnv = P.setEnv params
    , R.buildDepends = P.buildDepends params
    , R.relaxDepends = P.relaxDepends params
    , R.noClean = P.noClean params
    , R.extraPackages = P.extraPackages params
    , R.extraEssential = P.extraEssential params
    , R.omitEssential = P.omitEssential params
    , R.omitBuildEssential = P.omitBuildEssential params
    , R.baseRelease = P.baseRelease params
    , R.buildRelease = P.buildRelease params
    , R.doNotChangeVersion = P.doNotChangeVersion params
    , R.isDevelopmentRelease = P.isDevelopmentRelease params
    , R.releaseAliases = P.releaseAliases params
    , R.flushRoot = P.flushRoot params
    , R.cleanUp = P.cleanUp params
    , R.archList = P.archList params
    , R.flushPool = P.flushPool params
    , R.doUpload = P.doUpload params
    , R.doNewDist = P.doNewDist params
    , R.newDistProgram = P.newDistProgram params
    , R.uploadHost = P.uploadHost params
    , R.uploadURI = P.uploadURI params
    , R.buildURI = P.buildURI params
    , R.createRelease = P.createRelease params
    , R.ifSourcesChanged = P.ifSourcesChanged params
    , R.doSSHExport = P.doSSHExport params
    , R.autobuilderEmail = P.autobuilderEmail params
    }

defaultParams base tag email =
    R.ParamRec
    { R.verbosity = 0
    , R.topDirParam = Nothing
    , R.debug = False
    , R.dryRun = False
    , R.requiredVersion = [(parseDebianVersion "4.41", Nothing)]
    , R.showSources = False
    , R.showParams = False
    , R.flushAll = False
    , R.useRepoCache = False
    , R.sources = []
    , R.targets = []
    , R.goals = []
    , R.omitTargets = []
    , R.vendorTag = tag
    , R.extraReleaseTag = Nothing
    , R.flushSource = False
    , R.forceBuild = []
    , R.allowBuildDependencyRegressions = False
    , R.preferred = []
    , R.strictness = P.Moderate
    , R.setEnv = []
    , R.buildDepends = []
    , R.relaxDepends = G.RelaxInfo []
    , R.noClean = False
    , R.extraPackages = []
    , R.extraEssential = []
    , R.omitEssential = []
    , R.omitBuildEssential = False
    , R.baseRelease = SliceName {sliceName = base}
    , R.buildRelease = ReleaseName {relName = base ++ "-" ++ tag}
    , R.doNotChangeVersion = False
    , R.isDevelopmentRelease = False
    , R.releaseAliases = []
    , R.flushRoot = False
    , R.cleanUp = False
    , R.archList = [Binary "i386",Binary "amd64"]
    , R.flushPool = False
    , R.doUpload = False
    , R.doNewDist = False
    , R.newDistProgram = "newdist -v"
    , R.uploadHost = Nothing
    , R.uploadURI = Nothing
    , R.buildURI = Nothing
    , R.createRelease = []
    , R.ifSourcesChanged = SourcesChangedError
    , R.doSSHExport = False
    , R.autobuilderEmail = email
    }
