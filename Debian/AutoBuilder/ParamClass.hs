{-# LANGUAGE FlexibleInstances, PackageImports #-}
module Debian.AutoBuilder.ParamClass
    ( ParamClass(..)
    , CacheClass(..)
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
    , baseRelease
    , isDevelopmentRelease
    , relaxDepends
    , dropSuffix
    , dropOneSuffix
    , dropAllSuffixes
    ) where

import           Control.OldException (try, evaluate)
import           Control.Monad.State (get, put)
import	"mtl"	 Control.Monad.Trans (lift, liftIO)
import		 Data.List (isSuffixOf)
import		 Data.Maybe
import           Data.Map (fromList)
import		 Debian.Repo.Cache (SourcesChangedAction)
import           Debian.Repo(EnvRoot(EnvRoot), Arch, SliceName(..),
                             SliceList(..), NamedSliceList(..), ReleaseName, releaseName',
                             setRepoMap, parseSourcesList, verifySourcesList, repoSources)
import           Debian.Repo.Monad (AptIOT)
import           Debian.Repo.Types (ReleaseName(relName))
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

-- |An instance of 'ParamClass' contains the configuration parameters
-- for a run of the autobuilder.  Among other things, it defined a set
-- of target packages to build and a single build environment to build
-- them in.  (This typeclass and those belowe were used to transition
-- from a command line option based interface, they could probably be
-- replaced by records now.)  The methods are given in approximate
-- order of importance.
class ParamClass a where
    vendorTag :: a -> String
    -- ^ The string used to construct modified version numbers to identify
    -- them as part of your repository (rather than Debian's or Ubuntu's.)
    oldVendorTags :: a -> [String]
    -- ^ Additional vendor tags that should be treated as part of the local
    -- repository, and stripped when deciding the version number of the
    -- upstream source.
    autobuilderEmail :: a -> String
    -- ^ Email return address of autobuilder for use in generated
    -- changelog entries.
    releaseSuffixes :: a -> [String]
    -- ^ All build releases must have one of these suffixes.  When the
    -- suffix is stripped off the result is the corresponding base
    -- release.  We use ["-seereason", "-private"] for this value so
    -- we can build a public release based on any debian or ubuntu
    -- release, and a private release based on each public release.
    buildRelease :: a -> ReleaseName
    -- ^ The name of the release we will be uploading to.  It must end
    -- with one of the 'releaseSuffixes', and stripping off
    -- one of them results in the base release.
    uploadURI :: a -> Maybe URI
    -- ^ This URI is the address of the remote repository to which packages
    -- will be uploaded after a run with no failures, when the myDoUpload
    -- flag is true.  Packages are uploaded to the directory created by
    -- appending @\/incoming@ to this URI.  This is distinct from the
    -- local repository, where each packages is uploaded immediately after
    -- it is built for use as build dependencies of other packages during
    -- the same run.
    buildURI :: a -> Maybe URI
    -- ^ An alternate url for the same repository the @uploadURI@ points to,
    -- used for downloading packages that have already been installed
    -- there.
    targets :: a -> [Target]
    -- ^ The packages to build.  The 'Target' record includes the
    -- source package name, a string describing how the source is to
    -- be obtained, and a dependency relaxation list, a list of binary
    -- packages which normally would trigger a rebuild when they
    -- changed.  some methods for obtaining the source code of a
    -- package to be built.  See "Debian.AutoBuilder.BuildTarget" for
    -- information about the available target types.
    doUpload :: a -> Bool
    -- ^ After a successful build of all the targets, dupload all the
    -- packages in the local pool specified by the @uploadURI@
    -- argument to the corresponding repository.
    doNewDist :: a -> Bool
    -- ^ After uploading, run newdist on the remote repository
    -- incoming directory
    flushPool :: a -> Bool
    -- ^ Discard the packages in the local pool before building.  Use
    -- this when a bad package was uploaded to the local pool that
    -- you don't want uploaded to the remote pool.
    useRepoCache :: a -> Bool
    -- ^ Load the most recent cached repository information from
    -- @~\/.autobuilder\/repoCache@ and assume that it is still good -
    -- that no releases have been added or removed from the
    -- repositories listed.  This is usually safe and saves some
    -- time querying each remote repository before using it.
    forceBuild :: a -> [String]
    -- ^ Build the named source package(s) whether or not they seem
    -- to need it.
    doSSHExport :: a -> Bool
    -- ^ Try to set up ssh keys if upload host asks for a password.
    doHelp :: a -> Bool
    -- ^ Print a usage message and exit.

    -- THINGS THAT ARE OCCASIONALLY USEFUL

    goals :: a -> [String]
    -- ^ Specify a source package which we want to build, and stop
    -- once all goals are built.  If not given all targets are
    -- considered goals.  (As of version 4.41 this option is not be
    -- fully functional, sometimes specifying goals will prevent all
    -- the builds.)
    allowBuildDependencyRegressions :: a -> Bool
    -- ^ Normally, if a build dependency has an older version number
    -- than it did on a previous build, it is an error.  This
    -- generally means the sources.list is incorrect.  However, this
    -- flag can be necessary if a package gets withdrawn from the build
    -- or base release.
    setEnv :: a -> [String]
    -- ^ Set one or more environment variables during the build, e.g. [_$_]
    -- @setEnv = ["DEBIAN_KERNEL_JOBS=5"]@.
    dryRun :: a -> Bool
    -- ^ This flag says not to do anything that will affect the
    -- outside world, such as uploads and remote newdists.  However,
    -- the files in @~\/.autobuilder@ may still be modified when this
    -- is used.  It does avoids making extensive changes to the
    -- local repository by exiting as soon as a target it identified
    -- as needing to be built.
    showSources :: a -> Bool
    -- ^ Print the @sources.list@ for the build distro and exit.
    showParams :: a -> Bool
    -- ^ Print the expanded runtime parameter list and continue.
    flushAll :: a -> Bool
    -- ^ Remove and re-create the entire autobuilder working directory (topDir.)
    flushSource :: a -> Bool
    -- ^ Discard and re-download all source code before building.
    flushRoot :: a -> Bool
    -- ^ Discard and recreate the clean build environment.
    verbosity :: a -> Int
    -- ^ Higher numbers increase the amount of progress reporting.
    topDirParam :: a -> Maybe FilePath
    -- ^ Normally the autobuilder uses @$HOME\/.autobuilder@ for semi-permanent
    -- storage, use this flag to specify a different location.
    createRelease :: a -> [String]
    -- ^ Pass a @--create <name>@ argument to newdist to create a new
    -- release in the upload repository.
    doNotChangeVersion :: a -> Bool
    -- ^ DANGER!  Prevents any modification of the package's version
    -- number before building.  Normally a tag is added to signify the
    -- vendor and the base release of the package.  Using this option
    -- can lead to attempts to upload packages that are already
    -- present in the repository, or packages that are trumped by
    -- versions already uploaded to the release.

    -- THINGS THAT RARELY CHANGE

    sources :: a -> [(String, String)]
    -- ^ Specify all known @source.list@ files, associating a name
    -- with each one.  The names can be used in apt targets.
    globalRelaxInfo :: a -> [String]
    -- ^ A list of packages which will not trigger rebuilds when
    -- updated.  Used to avoid massive rebuilds when package which are
    -- build essential but are unlikely to affect the build, such as
    -- @tar@, are updated.
    strictness :: a -> Strictness
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
    extraPackages :: a -> [String]
    -- ^ Additional packages to include in the clean build environment.
    -- Adding packages here can speed things up when you are building many
    -- packages, because for each package it reverts the build environment
    -- to the clean environment and then installs all the build
    -- dependencies.  This only affects newly created environments, so if
    -- you change this value use the flushRoot option to get it to take
    -- effect.
    extraEssential :: a -> [String]
    -- ^ Specify extra packages to include as essential in the build
    -- environment.  This option was provided to add either upstart or
    -- sysvinit to the build when they ceased to be 'Required' packages.
    omitEssential :: a -> [String]
    -- ^ Specify packages for build-env to remove from the essential
    -- list even if they are marked essential
    omitBuildEssential :: a -> Bool
    -- ^ OBSOLETE: Don't automatically consider all the build
    -- essential packages to be build dependencies.  If you are
    -- working with an unstable repository where the core packages
    -- are undergoing frequent revisions, and you aren't worried
    -- that a new version of @tar@ is going to change the outcome of
    -- your builds, this option can reduce the number of pointless
    -- rebuilds.  (But try relaxDepends first.)
    developmentReleaseNames :: a -> [String]
    -- ^ The list of upstream release which are currently in
    -- development.  This means we the tag we add doesn't need to
    -- include @~<release>@, since there are no newer releases to
    -- worry about trumping.  Debian's @sid@ is always in this
    -- list, along with the development version of Ubuntu.
    releaseAliases :: a -> [(String, String)]
    -- ^ Use these aliases for the release name when constructing the
    -- vendor tag used in the version number extension of built
    -- packages.  For example, including the pair @("hardy-seereason",
    -- "hardy")@ here means that packages built for our
    -- @hardy-seereason@ release will be assigned version numbers with
    -- suffixes like @0seereason3~hardy5@ rather than
    -- @0seereason3~hardy-seereason5@ (the latter would be an illegal
    -- due to the dash.)
    archList :: a -> [Arch]
    -- ^ The list of architectures to prepare the repository to accept.
    newDistProgram :: a -> String
    -- ^ Use given executable as the newdist program, the program that
    -- runs on the upload host to install packages in the incoming
    -- directory to the repository.

    -- THINGS THAT ARE PROBABLY OBSOLETE

    requiredVersion :: a -> [(DebianVersion, Maybe String)]
    -- ^ Specifies the version of the library required.  (Obsolete?)
    debug :: a -> Bool
    -- ^ Unspecified debugging behavior.
    omitTargets :: a -> [String]
    extraReleaseTag :: a -> Maybe Int
    preferred :: a -> [String]
    -- ^ When selecting build dependencies, prefer this particular
    -- package over other alternatives that could fulfill the
    -- dependency, even if this package seems older than some other
    -- alternative.  For example, the c-compiler virtual package is
    -- provided by @gcc-3.3@, @gcc-3.4@, @gcc-4.0@, etc.  If @gcc-3.4@ is
    -- in this list, a dependency on c-compiler will choose @gcc-3.4@
    -- over the others if possible.
    buildDepends :: a -> [String]
    -- ^ Obsolete?  Add a missing build dependency.
    noClean :: a -> Bool
    cleanUp :: a -> Bool
    -- ^ Do a garbage collection on the local repository, move all
    -- unreferenced files to @removed@.  This is probably not a
    -- useful option, as the local repository is frequently removed.
    ifSourcesChanged :: a -> SourcesChangedAction
    -- ^ What to do if the sources.list changes in the
    -- configuration directory.  The argument may be
    --
    --  * 'SourcesChangedError' - (the default) print a message and exit, [_$_]
    --
    --  * 'SourcesChangedUpdate' - rewrite sources.list and update the environment, [_$_]
    --
    --  * 'SourcesChangedRemove' - discard and rebuild the environment

-- |Output a (somewhat) readable representation of the parameter set.
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
            , "oldVendorTags=" ++ take 120 (show (oldVendorTags x))
            , "extraReleaseTag=" ++ take 120 (show (extraReleaseTag x))
            , "flushSource=" ++ take 120 (show (flushSource x))
            , "forceBuild=" ++ take 120 (show (forceBuild x))
            , "allowBuildDependencyRegressions=" ++ take 120 (show (allowBuildDependencyRegressions x))
            , "preferred=" ++ take 120 (show (preferred x))
            , "strictness=" ++ take 120 (show (strictness x))
            , "setEnv=" ++ take 120 (show (setEnv x))
            , "buildDepends=" ++ take 120 (show (buildDepends x))
            , "globalRelaxInfo=" ++ take 120 (show (globalRelaxInfo x))
            , "noClean=" ++ take 120 (show (noClean x))
            , "extraPackages=" ++ take 120 (show (extraPackages x))
            , "extraEssential=" ++ take 120 (show (extraEssential x))
            , "omitEssential=" ++ take 120 (show (omitEssential x))
            , "omitBuildEssential=" ++ take 120 (show (omitBuildEssential x))
            , "buildRelease=" ++ take 120 (show (buildRelease x))
            , "releaseSuffixes=" ++ take 120 (show (releaseSuffixes x))
            , "developmentReleaseNames=" ++ take 120 (show (developmentReleaseNames x))
            , "doNotChangeVersion=" ++ take 120 (show (doNotChangeVersion x))
            , "releaseAliases=" ++ take 120 (show (releaseAliases x))
            , "flushRoot=" ++ take 120 (show (flushRoot x))
            , "cleanUp=" ++ take 120 (show (cleanUp x))
            , "archList=" ++ take 120 (show (archList x))
            , "flushPool=" ++ take 120 (show (flushPool x))
            , "doUpload=" ++ take 120 (show (doUpload x))
            , "doNewDist=" ++ take 120 (show (doNewDist x))
            , "newDistProgram=" ++ take 120 (show (newDistProgram x))
            , "uploadURI=" ++ take 120 (show (uploadURI x))
            , "buildURI=" ++ take 120 (show (buildURI x))
            , "createRelease=" ++ take 120 (show (createRelease x))
            --, "ifSourcesChanged=" ++ take 120 (show (ifSourcesChanged x))
            , "doSSHExport=" ++ take 120 (show (doSSHExport x))
            , "autobuilderEmail=" ++ take 120 (show (autobuilderEmail x))
            --, "baseRelease sources=\n" ++ show (lookup (sliceName (baseRelease x)) (sources x))
            ]

-- |Along with an instance of 'ParamClass' we need an instance of
-- 'CacheClass', which contains the result examining the local and
-- remote environment to determine the home directory and the status
-- of all the remote repositories we want to use.
data Cache
    = Cache { topDir' :: FilePath
            , allSources' :: [NamedSliceList]
            , buildRepoSources' :: SliceList
            }

-- |Create a Cache object from a parameter set.
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

-- |Create a type class for Cache objects so we can make a
-- (ParamClass, Cache) pair an instance of it.
class CacheClass a where
    topDir :: a -> FilePath
    allSources :: a -> [NamedSliceList]
    buildRepoSources :: a -> SliceList

instance CacheClass Cache where
    topDir = topDir'
    allSources = allSources'
    buildRepoSources = buildRepoSources'

-- |An instance of RunClass contains all the information we need to
-- run the autobuilder.
class (ParamClass a, CacheClass a) => RunClass a

-- |Make a ('ParamClass', 'CacheClass') pair an instance ParamClass,
-- CacheClass, and RunClass.
instance (ParamClass p) => RunClass (p, Cache)

instance CacheClass c => CacheClass (a, c) where
    topDir = topDir . snd
    allSources = allSources . snd
    buildRepoSources = buildRepoSources . snd

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
    oldVendorTags = oldVendorTags . fst
    extraReleaseTag = extraReleaseTag . fst
    flushSource = flushSource . fst
    forceBuild = forceBuild . fst
    allowBuildDependencyRegressions = allowBuildDependencyRegressions . fst
    preferred = preferred . fst
    strictness = strictness . fst
    setEnv = setEnv . fst
    buildDepends = buildDepends . fst
    globalRelaxInfo = globalRelaxInfo . fst
    noClean = noClean . fst
    extraPackages = extraPackages . fst
    extraEssential = extraEssential . fst
    omitEssential = omitEssential . fst
    omitBuildEssential = omitBuildEssential . fst
    buildRelease = buildRelease . fst
    releaseSuffixes = releaseSuffixes . fst
    developmentReleaseNames = developmentReleaseNames . fst
    doNotChangeVersion = doNotChangeVersion . fst
    releaseAliases = releaseAliases . fst
    flushRoot = flushRoot . fst
    cleanUp = cleanUp . fst
    archList = archList . fst
    flushPool = flushPool . fst
    doUpload = doUpload . fst
    doNewDist = doNewDist . fst
    newDistProgram = newDistProgram . fst
    uploadURI = uploadURI . fst
    buildURI = buildURI . fst
    createRelease = createRelease . fst
    ifSourcesChanged = ifSourcesChanged . fst
    doSSHExport = doSSHExport . fst
    doHelp = doHelp . fst
    autobuilderEmail = autobuilderEmail . fst

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
      homeDir = liftIO (try (getEnv "HOME")) >>= return . either (error "Environment variable @HOME@ not set") (++ "/.autobuilder")

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

-- | Packages uploaded to the build release will be compatible
-- with packages in this release.
baseRelease :: ParamClass a => a -> SliceName
baseRelease params =
    maybe (error $ "Unknown release suffix: " ++ rel) SliceName
              (dropOneSuffix (releaseSuffixes params) rel)
    where rel = (relName (buildRelease params))

dropSuffix suffix x = take (length x - length suffix) x

dropSuffixMaybe :: String -> String -> Maybe String
dropSuffixMaybe suffix x = if isSuffixOf suffix x then Just (dropSuffix suffix x) else Nothing

dropOneSuffix suffixes s =
    case catMaybes (map (`dropSuffixMaybe` s) suffixes) of
      [s'] -> Just s'
      _ -> Nothing

dropAllSuffixes :: [String] -> String -> String
dropAllSuffixes suffixes s = maybe s (dropAllSuffixes suffixes) (dropOneSuffix suffixes s)

-- | Signifies that the release we are building for is a development
-- (or unstable) release.  This means we the tag we add doesn't need
-- to include @~<release>@, since there are no newer releases to
-- worry about trumping.
isDevelopmentRelease params =
    elem (topReleaseName (relName (buildRelease params))) (developmentReleaseNames params)
    where
      topReleaseName name =
          foldr dropSuff name (releaseSuffixes params)
          where dropSuff suff name = if isSuffixOf suff name then dropSuffix suff name else name

-- | Prevent the appearance of a new binary package from
-- triggering builds of its build dependencies.  Optionally, a
-- particular source package can be specified whose rebuild will
-- be prevented.  This is used to break dependency loops, For
-- example, @Relax-Depends: ghc6 hscolour@ means \"even if ghc6
-- is rebuilt, don't rebuild hscolour even though ghc6 is one of
-- its build dependencies.\"
relaxDepends params =
    G.RelaxInfo $ map (\ target -> (G.BinPkgName target, Nothing)) (globalRelaxInfo params) ++
                  concatMap (\ target -> map (\ binPkg -> (G.BinPkgName binPkg, Just (G.SrcPkgName (sourcePackageName target))))
                             (relaxInfo target)) (targets params)
