{-# LANGUAGE FlexibleInstances, PackageImports, ScopedTypeVariables #-}
module Debian.AutoBuilder.Params
    ( Strictness(Strict, Moderate, Lax)
    , ParamRec(..)
    , CacheRec(..)
    , Package(..)
    , PackageFlag(..)
    , TargetSpec(..)

    , buildCache
    , prettyPrint
    , findSlice
    -- , dirtyRootOfRelease
    , cleanRootOfRelease
    , dirtyRoot
    , cleanRoot
    , localPoolDir
    , baseRelease
    , isDevelopmentRelease
    , relaxDepends
    -- , dropSuffix
    -- , dropOneSuffix
    -- , dropAllSuffixes

    , adjustVendorTag -- Export for testing
    ) where

import Control.Exception ( SomeException, try, evaluate )
import Control.Monad.State ( get, put )
import "mtl" Control.Monad.Trans ( liftIO )
import Data.List ( isSuffixOf )
import Data.Maybe ( catMaybes, fromJust )
import Data.Map ( fromList )
import qualified Data.Set as Set
import Debian.AutoBuilder.Spec (Spec)
import Debian.Release
    ( Arch,
      ReleaseName(relName),
      releaseName' )
import Debian.Sources
    ( SliceName(..) )
import Debian.Repo.Cache ( SourcesChangedAction )
import Debian.Repo
    ( EnvRoot(EnvRoot),
      NamedSliceList(..),
      parseSourcesList,
      verifySourcesList,
      repoSources )
import Debian.Repo.Monad ( AptIOT, setRepoMap )
import Debian.Repo.Types ( SliceList(..) )
import Debian.Version ( DebianVersion )
import Debian.URI ( URI, parseURI )
import qualified Debian.GenBuildDeps as G
    ( RelaxInfo(..), SrcPkgName(..), BinPkgName(..) )
import System.Directory
    ( createDirectoryIfMissing, getPermissions, writable )
import System.Environment ( getEnv )
import System.Unix.QIO (quieter, qPutStrLn)

-- import Debian.AutoBuilder.ParamClass as P ( ParamClass(..), Target, Strictness )

-- Lax: dependencies are installed into clean, clean synced to build for each target
-- Moderate: dependencies are installed into build, clean synced to build only at beginning of run
-- Strict: dependencies are installed into build, clean synced to build for each target
-- (Note that in the past eight years I've only ever used Moderate.  Who knows if the others work?)
data Strictness
    = Lax |		-- Let build dependencies accumulate
      Moderate |	-- Install only required build dependencies
      Strict		-- Create a new build environment for each package
      deriving Eq

instance Show Strictness where
    show Lax = "Lax"
    show Moderate = "Moderate"
    show Strict = "Strict"

data PackageFlag
    = RelaxDep String		-- ^ Build dependencies which be ignored when deciding whether to rebuild
    | ExtraDep String		-- ^ Build dependencies which should be added to the debian/control file
    | DebVersion String         -- ^ The exact debian version number to insert into the changelog.
    | Epoch Int                 -- ^ Set the epoch number in the version number
    deriving (Show, Eq, Ord)

data Package
    = Package
      { name :: String
      , spec :: Spec
      , flags :: [PackageFlag]
      } deriving (Show, Eq, Ord)

relaxInfo :: Package -> [String]
relaxInfo p = foldr f [] (flags p)
    where f (RelaxDep s) ss = s : ss
          f _ ss = ss

-- |An instance of 'ParamClass' contains the configuration parameters
-- for a run of the autobuilder.  Among other things, it defined a set
-- of target packages to build and a single build environment to build
-- them in.  (This typeclass and those belowe were used to transition
-- from a command line option based interface, they could probably be
-- replaced by records now.)  The methods are given in approximate
-- order of importance.
data ParamRec =
    ParamRec
    { vendorTag :: String
    -- ^ The string used to construct modified version numbers to identify
    -- them as part of your repository (rather than Debian's or Ubuntu's.)
    , oldVendorTags :: [String]
    -- ^ Additional vendor tags that should be treated as part of the local
    -- repository, and stripped when deciding the version number of the
    -- upstream source.
    , autobuilderEmail :: String
    -- ^ Email return address of autobuilder for use in generated
    -- changelog entries.
    , releaseSuffixes :: [String]
    -- ^ All build releases must have one of these suffixes.  When the
    -- suffix is stripped off the result is the corresponding base
    -- release.  We use ["-seereason", "-private"] for this value so
    -- we can build a public release based on any debian or ubuntu
    -- release, and a private release based on each public release.
    , buildRelease :: ReleaseName
    -- ^ The name of the release we will be uploading to.  It must end
    -- with one of the 'releaseSuffixes', and stripping off
    -- one of them results in the base release.
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
    , targets :: TargetSpec
    -- ^ The packages to build.  The 'Target' record includes the
    -- source package name, a string describing how the source is to
    -- be obtained, and a dependency relaxation list, a list of binary
    -- packages which normally would trigger a rebuild when they
    -- changed.  some methods for obtaining the source code of a
    -- package to be built.  See "Debian.AutoBuilder.BuildTarget" for
    -- information about the available target types.
    , doUpload :: Bool
    -- ^ After a successful build of all the targets, dupload all the
    -- packages in the local pool specified by the @uploadURI@
    -- argument to the corresponding repository.
    , doNewDist :: Bool
    -- ^ After uploading, run newdist on the remote repository
    -- incoming directory
    , flushPool :: Bool
    -- ^ Discard the packages in the local pool before building.  Use
    -- this when a bad package was uploaded to the local pool that
    -- you don't want uploaded to the remote pool.
    , useRepoCache :: Bool
    -- ^ Load the most recent cached repository information from
    -- @~\/.autobuilder\/repoCache@ and assume that it is still good -
    -- that no releases have been added or removed from the
    --,  repositories listed.  This is usually safe and saves some
    -- time querying each remote repository before using it.
    , forceBuild :: [String]
      -- ^,  Build the named source package(s) whether or not they seem
    -- to need it.
    , buildTrumped :: [String]
    -- ^ Build the named source package(s) whether or not they seem
    -- to be older than the version already in the repository.
    , doSSHExport :: Bool
    -- ^ Try to set up ssh keys if upload host asks for a password.
    , doHelp :: Bool
    -- ^ Print a usage message and exit.

    -- THINGS THAT ARE OCCASIONALLY USEFUL

    , goals :: [String]
    -- ^ Specify a source package which we want to build, and stop
    -- once all goals are built.  If not given all targets are
    -- considered goals.
    , allowBuildDependencyRegressions :: Bool
    -- ^ Normally, if a build dependency has an older version number
    -- than it did on a previous build, it is an error.  This
    -- generally means the sources.list is incorrect.  However, this
    -- flag can be necessary if a package gets withdrawn from the build
    -- or base release.
    , setEnv :: [String]
    -- ^ Set one or more environment variables during the build, e.g. [_$_]
    -- @setEnv = ["DEBIAN_KERNEL_JOBS=5"]@.
    , dryRun :: Bool
    -- ^ This flag says not to do anything that will affect the
    -- outside world, such as uploads and remote newdists.  However,
    -- the files in @~\/.autobuilder@ may still be modified when this
    -- is used.  It does avoids making extensive changes to the
    -- local repository by exiting as soon as a target it identified
    -- as needing to be built.
    , showSources :: Bool
    -- ^ Print the @sources.list@ for the build distro and exit.
    , showParams :: Bool
    -- ^ Print the expanded runtime parameter list and continue.
    , flushAll :: Bool
    -- ^ Remove and re-create the entire autobuilder working directory (topDir.)
    , flushSource :: Bool
    -- ^ Discard and re-download all source code before building.
    , flushRoot :: Bool
    -- ^ Discard and recreate the clean build environment.
    , verbosity :: Int
    -- ^ Higher numbers increase the amount of progress reporting.
    , topDirParam :: Maybe FilePath
    -- ^ Normally the autobuilder uses @$HOME\/.autobuilder@ for semi-permanent
    -- storage, use this flag to specify a different location.
    , createRelease :: [String]
    -- ^ Pass a @--create <name>@ argument to newdist to create a new
    -- release in the upload repository.
    , doNotChangeVersion :: Bool
    -- ^ DANGER!  Prevents any modification of the package's version
    -- number before building.  Normally a tag is added to signify the
    -- vendor and the base release of the package.  Using this option
    -- can lead to attempts to upload packages that are already
    -- present in the repository, or packages that are trumped by
    -- versions already uploaded to the release.
    , discard :: Set.Set String
    -- ^ When any of these targets become ready to build, fail them.
    -- This is to save time on targets we know will fail.
    , testWithPrivate :: Bool

    -- THINGS THAT RARELY CHANGE

    , sources :: [(String, String)]
    -- ^ Specify all known @source.list@ files, associating a name
    -- with each one.  The names can be used in apt targets.
    , globalRelaxInfo :: [String]
    -- ^ A list of packages which will not trigger rebuilds when
    -- updated.  Used to avoid massive rebuilds when package which are
    -- build essential but are unlikely to affect the build, such as
    -- @tar@, are updated.
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
    , includePackages :: [String]
    -- ^ Additional packages to include in the clean build environment.
    -- Adding packages here can speed things up when you are building many
    -- packages, because for each package it reverts the build environment
    -- to the clean environment and then installs all the build
    -- dependencies.  This only affects newly created environments, so if
    -- you change this value use the flushRoot option to get it to take
    -- effect.
    , excludePackages :: [String]
    -- ^ Specify packages for build-env to omit from the package list
    -- even if they are marked essential
    , components :: [String]
    -- ^ The list of components of the base repository, for Ubuntu this is
    -- main,restricted,universe,multiverse.
    , ghcVersion :: Maybe String
    -- ^ Until we can get the code to look for the compiler version in the
    -- changeroot, we use this to tell cabal-debian what compiler we are
    -- going to build with.
    , developmentReleaseNames :: [String]
    -- ^ The list of upstream release which are currently in
    -- development.  This means we the tag we add doesn't need to
    -- include @~<release>@, since there are no newer releases to
    -- worry about trumping.  Debian's @sid@ is always in this
    -- list, along with the development version of Ubuntu.
    , releaseAliases :: [(String, String)]
    -- ^ Use these aliases for the release name when constructing the
    -- vendor tag used in the version number extension of built
    -- packages.  For example, including the pair @("hardy-seereason",
    -- "hardy")@ here means that packages built for our
    -- @hardy-seereason@ release will be assigned version numbers with
    -- suffixes like @0seereason3~hardy5@ rather than
    -- @0seereason3~hardy-seereason5@ (the latter would be an illegal
    -- due to the dash.)
    , archList :: [Arch]
    -- ^ The list of architectures to prepare the repository to accept.
    , newDistProgram :: String
    -- ^ Use given executable as the newdist program, the program that
    -- runs on the upload host to install packages in the incoming
    -- directory to the repository.
    , requiredVersion :: [(DebianVersion, Maybe String)]
    -- ^ Specifies the version of the library required.

    -- THINGS THAT ARE PROBABLY OBSOLETE

    , debug :: Bool
    -- ^ Unspecified debugging behavior.
    , extraReleaseTag :: Maybe Int
    , preferred :: [String]
    -- ^ When selecting build dependencies, prefer this particular
    -- package over other alternatives that could fulfill the
    -- dependency, even if this package seems older than some other
    -- alternative.  For example, the c-compiler virtual package is
    -- provided by @gcc-3.3@, @gcc-3.4@, @gcc-4.0@, etc.  If @gcc-3.4@ is
    -- in this list, a dependency on c-compiler will choose @gcc-3.4@
    -- over the others if possible.
    , buildDepends :: [String]
    -- ^ Obsolete?  Add a missing build dependency.
    , noClean :: Bool
    , cleanUp :: Bool
    -- ^ Do a garbage collection on the local repository, move all
    -- unreferenced files to @removed@.  This is probably not a
    -- useful option, as the local repository is frequently removed.
    , ifSourcesChanged :: SourcesChangedAction
    -- ^ What to do if the sources.list changes in the
    -- configuration directory.  The argument may be
    --
    --  * 'SourcesChangedError' - (the default) print a message and exit, [_$_]
    --
    --  * 'SourcesChangedUpdate' - rewrite sources.list and update the environment, [_$_]
    --
    --  * 'SourcesChangedRemove' - discard and rebuild the environment
  } deriving Show

-- |Output a (somewhat) readable representation of the parameter set.
prettyPrint :: ParamRec -> String
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
            , "targets=" ++ take 120 (show (targets x))
            , "goals=" ++ take 120 (show (goals x))
            , "discard=" ++ take 120 (show (discard x))
            , "vendorTag=" ++ take 120 (show (vendorTag x))
            , "oldVendorTags=" ++ take 120 (show (oldVendorTags x))
            , "extraReleaseTag=" ++ take 120 (show (extraReleaseTag x))
            , "flushSource=" ++ take 120 (show (flushSource x))
            , "forceBuild=" ++ take 120 (show (forceBuild x))
            , "buildTrumped=" ++ take 120 (show (buildTrumped x))
            , "allowBuildDependencyRegressions=" ++ take 120 (show (allowBuildDependencyRegressions x))
            , "preferred=" ++ take 120 (show (preferred x))
            , "strictness=" ++ take 120 (show (strictness x))
            , "setEnv=" ++ take 120 (show (setEnv x))
            , "buildDepends=" ++ take 120 (show (buildDepends x))
            , "globalRelaxInfo=" ++ take 120 (show (globalRelaxInfo x))
            , "noClean=" ++ take 120 (show (noClean x))
            , "includePackages=" ++ take 120 (show (includePackages x))
            , "excludePackages=" ++ take 120 (show (excludePackages x))
            , "components=" ++ take 120 (show (components x))
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

-- |Create a Cache object from a parameter set.
buildCache :: ParamRec -> AptIOT IO CacheRec
buildCache params =
    do top <- liftIO $ computeTopDir params
       qPutStrLn ("Preparing autobuilder cache in " ++ top ++ "...")
       liftIO $ mapM_ (createDirectoryIfMissing True . ((top ++ "/") ++))
                  [".", "darcs", "deb-dir", "dists", "hackage", "localpools", "quilt", "tmp"]
       loadRepoCache top
       all <- mapM parseNamedSliceList (sources params)
       let uri = maybe (uploadURI params) Just (buildURI params)
       build <- maybe (return $ SliceList { slices = [] }) (repoSources Nothing) uri
       return $ CacheRec {params = params, topDir = top, allSources = all, buildRepoSources = build}
    where
      parseNamedSliceList (name, text) = 
          do sources <- (verifySourcesList Nothing . parseSourcesList) text
             return $ NamedSliceList { sliceListName = SliceName name, sliceList = sources }

data CacheRec
    = CacheRec
    { params :: ParamRec
    , topDir :: FilePath
    , allSources :: [NamedSliceList]
    , buildRepoSources :: SliceList
    }

-- |An instance of RunClass contains all the information we need to
-- run the autobuilder.
-- class (ParamClass a, CacheClass a) => RunClass a

-- |Make a ('ParamClass', 'CacheClass') pair an instance ParamClass,
-- CacheClass, and RunClass.
-- instance (ParamClass p) => RunClass (p, Cache)

loadRepoCache :: FilePath -> AptIOT IO ()
loadRepoCache top =
    do qPutStrLn "Loading repo cache..."
       state <- get
       uris <- liftIO $ try (readFile (top ++ "/repoCache")) >>=
               try . evaluate . either (\ (_ :: SomeException) -> []) read >>=
               return . either (\ (_ :: SomeException) -> []) id
       put (setRepoMap (fromList (map fixURI uris)) state)
    where
      fixURI (s, x) = (fromJust (parseURI s), x)

-- Compute the top directory, try to create it, and then make sure it
-- exists.  Then we can safely return it from topDir below.
computeTopDir :: ParamRec -> IO FilePath
computeTopDir params =
    try (maybe homeDir return (topDirParam params) >>= \ top ->
         createDirectoryIfMissing True top >>
         getPermissions top >>= return . writable >>= finish top) >>=
    either (\ (e :: SomeException) -> error (show e)) return
    where
      finish _ False = error "Cache directory not writable (are you root?)"
      finish top True = return top
      homeDir = getEnv "HOME" >>= return . (++ "/.autobuilder")

-- |Find a release by name, among all the "Sources" entries given in the configuration.
findSlice :: CacheRec -> SliceName -> Either String NamedSliceList
findSlice cache dist =
    case filter ((== dist) . sliceListName) (allSources cache) of
      [x] -> Right x
      [] -> Left ("No sources.list found for " ++ sliceName dist)
      xs -> Left ("Multiple sources.lists found for " ++ sliceName dist ++ "\n" ++ show (map (sliceName . sliceListName) xs))

dirtyRootOfRelease :: CacheRec -> ReleaseName -> EnvRoot
dirtyRootOfRelease cache distro =
    EnvRoot $ topDir cache ++ "/dists/" ++ releaseName' distro ++ "/build-" ++ (show (strictness (params cache)))
    --ReleaseCache.dirtyRoot distro (show (strictness params))

cleanRootOfRelease :: CacheRec -> ReleaseName -> EnvRoot
cleanRootOfRelease cache distro =
    EnvRoot $ topDir cache ++ "/dists/" ++ releaseName' distro ++ "/clean-" ++ (show (strictness (params cache)))
    --ReleaseCache.cleanRoot distro (show (strictness params))

dirtyRoot :: CacheRec -> EnvRoot
dirtyRoot cache = dirtyRootOfRelease cache (buildRelease (params cache))
    --EnvRoot $ topDir params ++ "/dists/" ++ show (buildRelease params) ++ "/build-" ++ (show (strictness params))

cleanRoot :: CacheRec -> EnvRoot
cleanRoot cache = cleanRootOfRelease cache (buildRelease (params cache))
    -- cleanRootOfRelease params (buildRelease params)

-- |Location of the local repository for uploaded packages.
localPoolDir :: CacheRec -> FilePath
localPoolDir cache = topDir cache ++ "/localpools/" ++ releaseName' (buildRelease (params cache))

-- | Packages uploaded to the build release will be compatible
-- with packages in this release.
baseRelease :: ParamRec -> SliceName
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

{-
dropAllSuffixes :: [String] -> String -> String
dropAllSuffixes suffixes s = maybe s (dropAllSuffixes suffixes) (dropOneSuffix suffixes s)
-}

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
relaxDepends params@(ParamRec {targets = TargetSet s}) =
    G.RelaxInfo $ map (\ target -> (G.BinPkgName target, Nothing)) (globalRelaxInfo params) ++
                  concatMap (\ target -> map (\ binPkg -> (G.BinPkgName binPkg, Just (G.SrcPkgName (name target))))
                             (relaxInfo target)) (Set.toList s)
relaxDepends _params = error "relaxDepends: invalid target set"

-- |Information about what targets to build are temporarily held in a
-- value of this type.  Once all the command line arguments have been
-- analyzed, this is transformed into a set of targets, which can be
-- used to implement the ParamClass "targets" method.
data TargetSpec
    = AllTargets
    | TargetNames (Set.Set String)
    | TargetSet (Set.Set Package)
    deriving Show

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
