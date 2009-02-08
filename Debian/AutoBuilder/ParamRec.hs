module Debian.AutoBuilder.ParamRec
    ( ParamRec(..)
    ) where

import qualified Debian.GenBuildDeps as G
import Debian.Repo
import Debian.URI
import Debian.Version
import Debian.AutoBuilder.ParamClass as P

data ParamRec =
    ParamRec
    { verbosity :: Int
    -- ^ Higher numbers increase the amount of progress reporting.
    , topDirParam :: Maybe FilePath
    -- ^ Normally the autobuilder uses @$HOME\/.autobuilder@ for semi-permanent
    -- storage, use this flag to specify a different location.
    , debug :: Bool
    -- ^ Unspecified debugging behavior.
    , dryRun :: Bool
    -- ^ This flag says not to do anything that will affect the
    -- outside world, such as uploads and remote newdists.  However,
    -- the files in @~\/.autobuilder@ may still be modified when this
    -- is used.  It does avoids making extensive changes to the
    -- local repository by exiting as soon as a target it identified
    -- as needing to be built.
    , requiredVersion :: [(DebianVersion, Maybe String)]
    , showSources :: Bool
    -- ^ Print the @sources.list@ for the build distro and exit.
    , showParams :: Bool
    -- ^ Print the expanded runtime parameter list and continue.
    , flushAll :: Bool
    -- ^ Remove and re-create the entire autobuilder working directory (topDir.)
    , useRepoCache :: Bool
    -- ^ Load the most recent cached repository information from
    -- @~\/.autobuilder\/repoCache@ and assume that it is still good -
    -- that no releases have been added or removed from the
    -- repositories listed.  This is usually safe and saves some
    -- time querying each remote repository before using it.
    , sources :: [(String, String)]
    -- ^ Specify all known @source.list@ files, associating a name
    -- with each one.  The names can be used in apt targets.
    , targets :: [Target]
    -- ^ Specify one or more build targets, methods for obtaining the
    -- source code of a package to be built.  See "Debian.AutoBuilder.BuildTarget"
    -- for information about the available target types.
    , goals :: [String]
    -- ^ Specify a source package which we want to build, and stop
    -- once all goals are built.  If not given all targets are
    -- considered goals.  (As of version 4.41 this option is not be
    -- fully functional, sometimes specifying goals will prevent all
    -- the builds.)
    , omitTargets :: [String]
    , vendorTag :: String
    -- ^ The string used to construct modified version numbers.
    , extraReleaseTag :: Maybe Int
    , flushSource :: Bool
    -- ^ Discard and re-download all source code before building.
    , forceBuild :: [String]
    -- ^ Build the named source package(s) whether or not they seem
    -- to need it.
    , allowBuildDependencyRegressions :: Bool
    -- ^ Normally, if a build dependency has an older version number
    -- than it did on a previous build, it is an error.  This
    -- generally means the sources.list is incorrect.  However, this
    -- flag can be necessary if a package gets withdrawn from the build
    -- or base release.
    , preferred :: [String]
    -- ^ When selecting build dependencies, prefer this particular
    -- package over other alternatives that could fulfill the
    -- dependency, even if this package seems older than some other
    -- alternative.  For example, the c-compiler virtual package is
    -- provided by @gcc-3.3@, @gcc-3.4@, @gcc-4.0@, etc.  If @gcc-3.4@ is
    -- in this list, a dependency on c-compiler will choose @gcc-3.4@
    -- over the others if possible.
    , strictness :: Strictness
    -- ^ Specify how strict to be about the creation of build
    -- environments, trading off correctness with speed.  In all
    -- cases, a clean build environment is always maintained, and
    -- copied before the package build is performed using @rsync@.
    -- 'Strict' means the clean build environment is discarded and
    -- recreated before each target is built.  'Moderate' means the
    -- clean build environment is kept between successive runs, and
    -- updated as necessary using @apt-get update@ and @apt-get
    -- dist-upgrade@.  'Lax' means that build dependencies are
    -- installed into the clean build environment so that they
    -- accumulate across runs.
    , setEnv :: [String]
    -- ^ Set one or more environment variables during the build, e.g. [_$_]
    -- @setEnv = ["DEBIAN_KERNEL_JOBS=5"]@.
    , buildDepends :: [String]
    -- ^ Obsolete?  Add a missing build dependency.
    , relaxDepends :: G.RelaxInfo
    -- ^ Prevent the appearance of a new binary package from
    -- triggering builds of its build dependencies.  Optionally, a
    -- particular source package can be specified whose rebuild will
    -- be prevented.  This is used to break dependency loops, For
    -- example, @Relax-Depends: ghc6 hscolour@ means \"even if ghc6
    -- is rebuilt, don't rebuild hscolour even though ghc6 is one of
    -- its build dependencies.\"
    , noClean :: Bool
    , extraPackages :: [String]
    -- ^ Additional packages to include in the clean build environment.
    -- Adding packages here can speed things up when you are building many
    -- packages, because for each package it reverts the build environment
    -- to the clean environment and then installs all the build
    -- dependencies.  This only affects newly created environments, so if
    -- you change this value use the flushRoot option to get it to take
    -- effect.
    , extraEssential :: [String]
    -- ^ Specify extra packages to include as essential in the build
    -- environment.  This option was provided to add either upstart or
    -- sysvinit to the build when they ceased to be 'Required' packages.
    , omitEssential :: [String]
    -- ^ Specify packages for build-env to remove from the essential
    -- list even if they are marked essential
    , omitBuildEssential :: Bool
    -- ^ OBSOLETE: Don't automatically consider all the build
    -- essential packages to be build dependencies.  If you are
    -- working with an unstable repository where the core packages
    -- are undergoing frequent revisions, and you aren't worried
    -- that a new version of @tar@ is going to change the outcome of
    -- your builds, this option can reduce the number of pointless
    -- rebuilds.  (But try relaxDepends first.)
    , baseRelease :: SliceName
    -- ^ Packages uploaded to the build release will be compatible
    -- with packages in this release.
    , buildRelease :: ReleaseName
    -- ^ The name of the release we will be uploading to.
    , doNotChangeVersion :: Bool
    -- ^ Don't modify the package's version in any way before
    -- building.  Normally a tag is added to signify the vendor and
    -- the base release of the package.  Using this option can lead
    -- to attempts to upload packages that are already present in
    -- the repository, or packages that are trumped by versions
    -- already uploaded to the release.
    , isDevelopmentRelease :: Bool
    -- ^ Signifies that the release we are building for is a development
    -- (or unstable) release.  This means we the tag we add doesn't need
    -- to include @~<release>@, since there are no newer releases to
    -- worry about trumping.
    , releaseAliases :: [(String, String)]
    -- ^ Use these aliases for the release name when constructing the
    -- vendor tag used in the version number extension of built
    -- packages.
    , flushRoot :: Bool
    -- ^ Discard and recreate the clean build environment.
    , cleanUp :: Bool
    -- ^ Do a garbage collection on the local repository, move all
    -- unreferenced files to @removed@.  This is probably not a
    -- useful option, as the local repository is frequently removed.
    , archList :: [Arch]
    -- ^ The list of architectures to prepare the repository to accept.
    , flushPool :: Bool
    -- ^ Discard the packages in the local pool before building.  Use
    -- this when a bad package was uploaded to the local pool that
    -- you don't want uploaded to the remote pool.
    , doUpload :: Bool
    -- ^ After a successful build of all the targets, dupload all the
    -- packages in the local pool specified by the @uploadURI@
    -- argument to the corresponding repository.
    , doNewDist :: Bool
    -- ^ After uploading, run newdist on the remote repository
    -- incoming directory
    , newDistProgram :: String
    -- ^ Use given executable as the newdist program, the program that
    -- runs on the upload host to install packages in the incoming
    -- directory to the repository.
    , uploadHost :: Maybe String
    , uploadURI :: Maybe URI
    -- ^ This URI is the address of the remote repository to which packages
    -- will be uploaded after a run with no failures, when the myDoUpload
    -- flag is true.  Packages are uploaded to the directory created by
    -- appending @\/incoming@ to this URI.  This is distinct from the
    -- local repository, where each packages is uploaded immediately after
    -- it is built for use as build dependencies of other packages during
    -- the same run.
    , buildURI :: Maybe URI
    -- ^ An alternate url for the same repository the @uploadURI@ points to,
    -- used for downloading packages that have already been installed
    -- there.
    , createRelease :: [String]
    -- ^ Pass a @--create <name>@ argument to newdist to create a new
    -- release in the upload repository.
    , ifSourcesChanged :: SourcesChangedAction
    -- ^ What to do if the sources.list changes in the
    -- configuration directory.  The argument may be
    --
    --  * 'SourcesChangedError' - (the default) print a message and exit, [_$_]
    --
    --  * 'SourcesChangedUpdate' - rewrite sources.list and update the environment, [_$_]
    --
    --  * 'SourcesChangedRemove' - discard and rebuild the environment
    , doSSHExport :: Bool
    -- ^ Try to set up ssh keys if upload host asks for a password.
    , autobuilderEmail :: String
    -- ^ Email address of autobuilder for use in generated changelog entries.
    } deriving Show 

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
