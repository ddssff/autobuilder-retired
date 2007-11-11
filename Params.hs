-- |This module examines the command line arguments, the configuration
-- files, and perhaps other aspects of the computing environment
-- (e.g. the output of dpkg-architecture) and computes the run time
-- parameters for the program.
--
-- Author: David Fox <ddssff@gmail.com>
module Params
    (-- * Runtime Parameters
     Params(..),
     optSpecs,
     params,			-- IO [Params],
     prettyPrint,		-- Params -> String
     usage,			-- String -> String
     -- * Global Parameters
     topDir,			-- Params -> TopDir
     debug,			-- Params -> Bool
     Params.dryRun,		-- Params -> Bool
     style,			-- Params -> [Progress.Style]
     requiredVersion,		-- Params -> [(DebianVersion, Maybe String)]
     showSources,		-- Params -> Bool
     showParams,		-- Params -> Bool
     flushAll,
     -- * Obtaining and preparing target source
     Params.sources,		-- Params -> [String]	(Use distro)
     targets,			-- Params -> [String]
     omitTargets,		-- Params -> [String]
     noVersionFix,		-- Params -> Bool
     vendorTag,			-- Params -> String
     --releaseTag,		-- Params -> Int
     defaultTag,		-- Params -> VersionTag
     flushSource,		-- Params -> Bool
     -- * Build Environment
     forceBuild,		-- Params -> Bool
     preferred,			-- Params -> [String]
     Params.cleanRoot,		-- Params -> EnvRoot
     Params.dirtyRoot,		-- Params -> EnvRoot
     cleanRootOfRelease,
     Strictness(Strict, Moderate, Lax),
     strictness,		-- Params -> Strictness
     setEnv,			-- Params -> [String]
     buildDepends,		-- Params -> [String]
     relaxDepends,		-- Params -> [String]
     noClean,			-- Params -> Bool
     extraPackages,		-- Params -> [String]
     extraEssential,
     omitEssential,		-- Params -> [String]
     omitBuildEssential,	-- Params -> Bool
     baseRelease,
     buildRelease,
     flushRoot,			-- Params -> Bool
     -- * Local repository
     cleanUp,			-- Params -> Bool
     archList,			-- Params -> [Arch]
     flushPool,			-- Params -> Bool
     Params.localPoolDir,	-- Params -> FilePath
     -- * Uploading
     doUpload,			-- Params -> Bool
     doNewDist,			-- Params -> Bool
     newDistProgram,
     uploadHost,		-- Params -> Maybe String (derived from uploadUrl)
     uploadURI,			-- Params -> Maybe URI
     createRelease,
     ifSourcesChanged,
     doSSHExport,
     autobuilderEmail,		-- Params -> String
     -- * Other
     findSlice
    ) where

import		 Config hiding (usageInfo)
import		 Control.Exception
import		 Data.List
import		 Data.Maybe
import		 Debian.Cache
import		 Debian.IO
import		 Debian.Slice
import		 Debian.Types
import		 Debian.Version
import		 Debian.VersionPolicy
import		 Extra.Misc
import		 Network.URI
import qualified Config as P (usageInfo, ParamDescr(Param), shortOpts, longOpts, argDescr, description, names, values)
import qualified Data.Map as Map
import qualified System.IO as IO
import		 System.Console.GetOpt
import		 System.Directory
import		 System.Environment as Environment
import		 Text.Regex as Regex

data Params
    = Params { flags :: Map.Map String [String]
             , allSources :: [NamedSliceList]
             , buildRepoSources :: SliceList
             }

instance ParamSet Params where
    values params descr =
        concat (map (\ name -> Map.findWithDefault [] name (flags params)) (names descr))

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

-- |Compute and return all the run time parameters by expanding the list of flags,
-- generally computed from the command line arguments, using the Name\/Use macro
-- expansion mechanism.  The result is a Map from a parameter names to a list of the
-- values that were found for that parameter.
--
-- The order of the list of values for a given parameter is well
-- defined.  Values from an expansion of a Use parameter will appear
-- later in the list than values given at a higher level.  Thus,
-- command line parameters will always appear first, and the values
-- given in the common section of the example configuration will
-- always appear last.  IF two values for a parameter are given in the
-- same paragraph of the configuration file, their order is preserved.
--
-- The appName string is used to construct the usage message and
-- candidates for the configuration directory path.
params :: Int -> String -> [Flag] -> AptIO [Params]
params verbosity appName flags =
    do flagLists <- io $ Config.computeConfig verbosity appName flags
       --vPutStrLn 2 IO.stderr ("flagLists: " ++ show flagLists)
       flagMaps <- io (mapM computeTopDir (map (listMap . pairsFromFlags) flagLists))
       --vPutStrLn 2 IO.stderr ("flagMaps: " ++ show flagMaps)
       -- Make sure the topdir for each set of parameters exists and
       -- is writable.  If not, we won't be able to update any environments
       -- and none of the information we get will be accurate.
       params <- mapM makeFlagSet flagMaps
       mapM_ (vPutStrLn 2) ("buildRepoSources:" : map ((" " ++) . show . buildRepoSources) params)
       {- mapM verifySources params -}
       return params
    where
      makeFlagSet flags =
          do allSources <- allSourcesOfFlags flags
             buildRepoSources <- 
                     case nub (Map.findWithDefault [] "Build-URI" flags) of
                       [] -> case nub (Map.findWithDefault [] "Upload-URI" flags) of
                               [] -> return SliceList { slices = [] }
                               [x] -> maybe (error $ "Invalid Upload-URI parameter: " ++ show x) (repoSources Nothing) (parseURI x)
                               xs -> error $ "Multiple Upload-URI parameters: " ++ show xs
                       [x] -> maybe (error $ "Invalid Build-URI parameter: " ++ show x) (repoSources Nothing) (parseURI x)
                       xs -> error $ "Multiple Build-URI parameters: " ++ show xs
             return $ Params { flags = flags
                             , allSources = allSources
                             , buildRepoSources = buildRepoSources }        
      pairsFromFlags (Value k a : etc) = (k, a) : pairsFromFlags etc
      pairsFromFlags (_ : etc) = pairsFromFlags etc
      pairsFromFlags [] = []
      allSourcesOfFlags flags = mapM parseNamedSliceList' (sources flags)
      sources flags = Map.findWithDefault [] "Sources" flags

buildURIOpt :: ParamDescr
buildURIOpt = Param [] ["build-uri"] ["Build-URI"] (ReqArg (Value "Build-URI") "[SOURCES.LIST LINE]")
              (text ["An alternate url for the same repository the upload-uri points to, used for",
                     "downloading packages that have already been installed there."])

commentOpt = Param [] [] ["Comment"] (ReqArg (Value "Comment") "COMMENT")
	     "No effect.  Used to document the configuration file."

versionOpt = Param [] ["version"] ["Version"] (NoArg (Value "Version" "yes")) "Print the package version number and exit."

-- |Command line option specifications
optSpecs :: [ParamDescr]
optSpecs = globalOpts ++ sourceOpts ++ buildOpts ++ localRepoOpts ++ uploadOpts

globalOpts :: [ParamDescr]
globalOpts =
    [helpOpt,
     debugOpt,
     versionOpt,
     requiredVersionOpt,
     topDirOpt,
     dryRunOpt,
     commentOpt,
     verbosityOpt,
     flushAllOpt,
     showParamsOpt,
     showSourcesOpt,
     offlineOpt,
     styleOpt]

-- |Obtaining and preparing target source
sourceOpts :: [ParamDescr]
sourceOpts =
    [sourcesOpt,
     flushSourceOpt, 
     noVersionFixOpt,
     vendorTagOpt,
     releaseTagOpt,
     targetsOpt,
     omitTargetsOpt]

-- |Build Environment
buildOpts :: [ParamDescr]
buildOpts =
    [forceBuildOpt,
     laxOpt, moderateOpt, strictOpt,
     preferOpt,
     buildDependsOpt,
     setEnvOpt,
     noCleanOpt,
     baseReleaseOpt,
     buildReleaseOpt,
     ifSourcesChangedOpt,
     relaxDependsOpt,
     omitBuildEssentialOpt,
     extraEssentialOpt,
     omitEssentialOpt,
     extraPackagesOpt,
     flushRootOpt]

-- |Local repository
localRepoOpts :: [ParamDescr]
localRepoOpts =
    [flushPoolOpt,
     cleanUpOpt,
     archListOpt]

-- |Uploading
uploadOpts :: [ParamDescr]
uploadOpts =
    [uploadURIOpt,
     buildURIOpt,
     doUploadOpt,
     doNewDistOpt,
     newDistProgramOpt,
     createReleaseOpt,
     doSSHExportOpt,
     autobuilderEmailOpt]

-- |Usage message
usage :: String -> String
usage appName
    = P.usageInfo ("\nUsage: " ++ appName ++ " [OPTIONS] [SECTION NAMES]\n\n" ++
                   "See the --config option for a description of the locations\n" ++
                   "of the configuration file or directory.  See the --use option\n" ++
                   "for a description of the treatment of options that occur\n" ++
                   "repeatedly.  Any arguments not associated with a flag are treated\n" ++
                   "as additional --use arguments.\n\n" ++
                   "EXAMPLES:\n" ++
                   "  sudo autobuilder build-feisty-common --target apt:sid:haskell-hsql --flush-pool\n" ++
                   "  sudo autobuilder build-feisty-common --do-upload\n\n" ++
                   "GLOBAL OPTIONS:\n") (Config.optBaseSpecs appName ++ globalOpts) ++
      P.usageInfo "\nOBTAINING AND PREPARING TARGET SOURCE CODE:\n" sourceOpts ++
      P.usageInfo "\nCREATING AND USING THE BUILD ENVIRONMENT:\n" buildOpts ++
      P.usageInfo "\nMANAGING THE LOCAL TEMPORARY PACKAGE REPOSITORY:\n" localRepoOpts ++
      P.usageInfo "\nUPLOADING PACKAGES TO THE REMOTE REPOSITORY:\n" uploadOpts

-- |A function to compactly display a parameter set, cutting off any
-- long strings.
prettyPrint :: Params -> String
prettyPrint params =
    concat (reverse (Map.foldWithKey (\ k xs s -> format k xs ++ s) [] (flags params)))
    where
      format k xs = map (\ x -> ("  " ++ k ++ ": " ++ (abbrev . escape) x ++ "\n")) xs
      escape s =
          case break (== '\n') s of
            (s, []) -> s
            (s, (_ : etc)) -> s ++ "\\n" ++ escape etc
      abbrev s =
          case length s of
            n | n > 50 -> take 50 s ++ "..."
	      | otherwise -> s

topDirDefault = "/var/cache/autobuilder"

-- | The top of the directory tree where the autobuilder will
-- create its information cache.
topDir :: Params -> FilePath
topDir params = head (Map.findWithDefault [topDirDefault] "Top-Dir" (flags params))

topDirOpt = Param [] ["top-dir"] ["Top-Dir"] (ReqArg (Value "Top-Dir") "PATH")
            (text ["The directory the program will use for its working storage,",
                   "default: " ++ topDirDefault])

-- Compute the top directory, try to create it, and then make sure it
-- exists.  Then we can safely return it from topDir below.
computeTopDir :: Map.Map String [String] -> IO (Map.Map String [String])
computeTopDir params =
    do
      case Map.member "Top-Dir" params of
        True -> return params
        False -> do
          top <- try (getEnv "HOME") >>= return . either (\ _ -> topDirDefault) (++ "/.autobuilder")
          try $ createDirectoryIfMissing True top
          result <- try $ getPermissions top >>= return . writable
          case result of
            Left _ -> error $ "Could not create cache directory " ++ top ++ " (are you root?)"
            Right False ->
                do
                  -- putStrLn (top ++ ": ok (read only)") -- error "Cache directory not writable (are you root?)"
                  return $ Map.insert "Top-Dir" [top] params
            Right True ->
                do
                  -- putStrLn (top ++ ": ok")
                  return $ Map.insert "Top-Dir" [top] params

-- |Find a release by name, among all the "Sources" entries given in the configuration.
findSlice :: Params -> SliceName -> Either String NamedSliceList
findSlice params dist =
    case filter ((== dist) . sliceListName) (allSources params) of
      [x] -> Right x
      [] -> Left ("No sources.list found for " ++ show dist)
      xs -> Left ("Multiple sources.lists found for " ++ show dist ++ "\n" ++ show (map (sliceName . sliceListName) xs))

-- |The string used to construct modified version numbers.
vendorTag :: Params -> String
vendorTag params =
    case values params vendorTagOpt of
      [] -> error "Missing Vendor-Tag parameter, can't generate version numbers."
      tag : _ -> tag
vendorTagOpt = Param [] ["vendor-tag"] ["Vendor-Tag"] (ReqArg (Value "Vendor-Tag") "TAG")
               "The string used to construct modified version numbers"

defaultTag :: Params -> VersionTag
defaultTag params =
    case values params defaultTagOpt of
      [] -> BuildTag (vendorTag params) 0
      _ -> ReleaseBuildTag (releaseTag params) (vendorTag params) 0
defaultTagOpt = Param [] ["release-build-tag"] ["Release-Build-Tag"] (NoArg (Value "Release-Build-Tag" "yes"))
                "Use release tags of the form r0vendor3 rather than simply vendor3."

-- |These are vendor tags that, along with the one returned by
-- vendorTag, will be stripped off before the new version number is
-- constructed.  This is used when you decide to change vendor tags
-- and there are still packages in your repository with the old vendor
-- tag.  (Deprecated - it is dangerous to strip things off the version
-- number automatically because you may end up with a version number
-- that is too old.)
--stripVendorTags :: Params -> [String]
--stripVendorTags params = Map.findWithDefault [] "Strip-Vendor-Tag" (flags params)

releaseTag :: Params -> Int
releaseTag params = head ((map read (values params releaseTagOpt)) ++ [0])
releaseTagOpt = Param [] ["release-tag"] ["Release-Tag"] (ReqArg (Value "Release-Tag") "NUMBER")
                ("A number used to construct modified version numbers, default: 0")

extraEssential :: Params -> [String]
extraEssential params = values params extraEssentialOpt

extraEssentialOpt =
    P.Param { P.shortOpts = []
            , P.longOpts = ["extra-essential"]
            , P.argDescr = ReqArg (Value "ExtraEssential") "PACKAGE"
            , P.description =
                (text ["Specify an extra package to include as essential in the build",
                       "environment.  This option was provided to add either upstart or",
                       "sysvinit to the build when they ceased to be 'Required' packages."])
{-	    , P.option = Option [] ["extra-essential"] (ReqArg (Value "Extra-Essential") "PACKAGE")
                         (text ["Specify an extra package to include as essential in the build",
                                "environment.  This option was provided to add either upstart or",
                                "sysvinit to the build when they ceased to be 'Required' packages."]) -}
            , P.names = ["Extra-Essential"] }

omitEssential :: Params -> [String]
omitEssential params = values params omitEssentialOpt
omitEssentialOpt = Param [] ["omit-essential"] ["Omit-Essential"] (ReqArg (Value "Omit-Essential") "PACKAGE")
                   (text ["Specify a package to tell build-env to remove from the",
                          "essential list even if it is marked essential"])

extraPackages :: Params -> [String]
extraPackages params = values params extraPackagesOpt
extraPackagesOpt = Param [] ["extra-package"] ["Extra-Package"] (ReqArg (Value "Extra-Package") "PACKAGE")
                   "Additional packages to include in the build environment."


-- |Return the value of a strictness flag (--strict, --moderate, --lax)
strictness :: Params -> Strictness
strictness params =
    case (values params laxOpt, values params moderateOpt, values params strictOpt) of
      ([], [], []) -> Moderate
      (_, [], []) -> Lax
      ([], [], _) -> Strict
      ([], _, []) -> Moderate
      _ -> error "Conflicting strictness options"

laxOpt = Param [] ["lax"] ["Lax"] (NoArg (Value "Strictness" "lax"))
	 (text ["Specify how strict to be about the creation of build environments.",
		"A clean build environment is always maintained, and copied before",
		"the package build is performed using rsync.  'Strict' means the",
		"clean build environment is discarded and recreated before each",
                "target is built.  'Moderate' means the clean build environment is",
                "kept between successive runs, and updated as necessary using",
                "'apt-get update' and 'apt-get dist-upgrade'.  'Lax' means that ",
                "build dependencies are installed into the clean build environment",
                "so that they accumulate across runs."])
moderateOpt = Param [] ["moderate"] ["Moderate"] (NoArg (Value "Strictness" "moderate")) "save build environment between targets"
strictOpt = Param [] ["strict"] ["Strict"] (NoArg (Value "Strictness" "strict")) "recreate build environment for each target"

-- New - accessors for Params

{-
help :: Params -> Bool
help params = values params helpOpt /= []
-}
helpOpt = Param [] ["help"] ["Help"] (NoArg (Value "Help" "yes")) "Print usage message."

debug :: Params -> Bool
debug params = values params debugOpt /= []
debugOpt = Param [] ["debug"] ["Debug"] (NoArg (Value "Debug" "yes"))
           "Unspecified debugging behavior."

-- The --required-version parameter gives the minimum version number of
-- the autobuilder and optionally a message explaining why an upgrade
-- is necessary.
requiredVersion :: Params -> [(DebianVersion, Maybe String)]
requiredVersion params =
    map parseVersionAndReason (values params requiredVersionOpt)
    where
      parseVersionAndReason s =
          case break (\ a -> elem a " \t\n") s of
            (v, []) -> (parseDebianVersion v, Nothing)
            (v, r) -> (parseDebianVersion v, Just r)
requiredVersionOpt = Param [] ["required-version"] ["Required-Version"] (ReqArg (Value "Required-Version") "VERSION [REASON]")
                     "Exit with a message if the version number of the autobuilder is too low."

-- |Unimplemented: this would allow off-line development, though it is
-- not clear exactly how.
{-
offline :: Params -> Bool
offline params = values params offlineOpt /= []
-}
offlineOpt = Param [] ["offline"] ["Offline"] (NoArg (Value "Offline" "yes"))
 	     "Unimplemented: work offline, don't issue commands that use the network (like apt-get)"

-- |Don't save the version numbers of the essential and
-- build-essential packages to the package's revision string, only the
-- build dependencies explicitly mentioned in the package's control
-- file.
omitBuildEssential :: Params -> Bool
omitBuildEssential params = values params omitBuildEssentialOpt /= []
omitBuildEssentialOpt = Param [] ["omit-build-essential"] ["Omit-Build-Essential"] (NoArg (Value "Omit-Build-Essential" "yes"))
                        (text ["Don't automatically consider all the build essential packages to be build",
                               "dependencies.  If you are working with an unstable repository where the",
                               "core packages are undergoing frequent revisions, and you aren't worried",
                               "that a new version of 'tar' is going to change the outcome of your builds,",
                               "this option can reduce the number of pointless rebuilds."])

-- |What this flag should mean in this program is somewhat unclear.
-- It may be removed in the future.
dryRun :: Params -> Bool
dryRun params = values params dryRunOpt /= []
dryRunOpt = Param ['n'] ["dry-run"] ["Dry-Run"] (NoArg (Value "Dry-Run" "yes"))
	    (text ["This flag says not to do anything that will affect the outside",
                   "world, such as uploads and remote newdists.  However, the",
                   "files in ~/.autobuilder may still be modified when this is used.",
                   "It does avoids making extensive changes to the local repository",
                   "by exiting as soon as a target it identified as needing to be",
                   "built."])

-- |Pass the -nc flag to dpkg-buildpackage.  (This won't work until
-- the strictness levels are modified so there is some way to build
-- without first syncing the clean environment into the build.)
noClean :: Params -> Bool
noClean params = values params noCleanOpt /= []
noCleanOpt = Param [] ["no-clean"] ["No-Clean"] (NoArg (Value "No-Clean" "yes"))
	     (text ["Run dpkg-buildpackage with the -nc argument.  This also disables",
                    "syncing with the clean source tree.  This should only be used for",
                    "debugging the autobuilder or for debugging the package build.  To",
                    "edit the package you need to find the work directory in the cached",
                    "build and make your edits there.  Then you will need to check them",
                    "back into your revision control system."])

forceBuild :: Params -> Bool
forceBuild params = values params forceBuildOpt /= []
forceBuildOpt = Param [] ["force-build"] ["Force-Build"] (NoArg (Value "Force-Build" "yes")) 
                "Build all targets whether or not they seems to need it."

-- | Upload the packages in the given local repository to the
-- corresponding remost repository.
doUpload :: Params -> Bool
doUpload params = values params doUploadOpt /= []
doUploadOpt = Param [] ["do-upload"] ["Do-Upload"] (NoArg (Value "Do-Upload" "yes"))
              (text ["After building any specified targets, dupload all the packages",
                     "in the local pool specified by the --upload-url argument to the",
                     "corresponding repository."])

doNewDist :: Params -> Bool
doNewDist params = values params doNewDistOpt /= []
doNewDistOpt = Param [] ["do-newdist"] ["Do-NewDist"] (NoArg (Value "Do-NewDist" "yes"))
               "Run newdist on the remote repository incoming directory"

newDistProgram :: Params -> String
newDistProgram params = case nub $ values params newDistProgramOpt of
                          [] -> "newdist -v"
                          [x] -> x
                          xs -> error $ "Multiple conflicting values for NewDist-Program: " ++ show xs
newDistProgramOpt = Param [] ["newdist-program"] ["Newdist-Program"] (ReqArg (Value "NewDist-Program") "PATH")
               "Use given executable as the newdist program (default: 'newdist'.)"

showSources :: Params -> Bool
showSources params = values params showSourcesOpt /= []
showSourcesOpt = Param [] ["show-sources"] ["Show-Sources"] (NoArg (Value "Show-Sources" "yes"))
                 "Print the sources.list for the build distro and exit"

showParams :: Params -> Bool
showParams params = values params showParamsOpt /= []
showParamsOpt = Param [] ["show-params"] ["Show-Params"] (NoArg (Value "Show-Params" "yes"))
                "Print the expanded runtime parameter list and continue."

cleanUp :: Params -> Bool
cleanUp params = values params cleanUpOpt /= []
cleanUpOpt = Param [] ["clean-up"] ["Clean-Up"] (NoArg (Value "Clean-Up" "yes"))
             (text ["Do a garbage collection on the local repository, move",
                    "all unreferenced files to 'removed'."])

doSSHExport :: Params -> Bool
doSSHExport params = values params doSSHExportOpt /= []
doSSHExportOpt = Param [] ["ssh-export"] ["SSH-Export"] (NoArg (Value "SSH-Export" "yes"))
                 "Try to set up ssh keys if upload host asks for a password."
 
ifSourcesChanged :: Params -> SourcesChangedAction
ifSourcesChanged params =
    let xs = values params ifSourcesChangedOpt in
    case xs of
      (first : etc) | not (all (== first) etc) -> error ("Conflicting values for If-Sources-Changed: " ++ show xs)
      [] -> SourcesChangedError
      ("error" : _) -> SourcesChangedError
      ("update" : _) -> UpdateSources
      ("remove" : _) -> RemoveRelease
      (other : _) -> error ("Invalid argument to If-Sources-Changed: " ++ other)
ifSourcesChangedOpt = Param [] ["if-sources-changed"] ["If-Sources-Changed"] (ReqArg (Value "If-Sources-Changed") "ACTION")
                      (text ["What to do if the sources.list changes in the",
                             "configuration directory.  The argument may be",
                             "error - (the default) print a message and exit, ",
                             "update - rewrite sources.list and update the environment, ",
                             "remove - discard and rebuild the environment"])

flushRoot :: Params -> Bool
flushRoot params = values params flushRootOpt /= []
flushRootOpt = Param [] ["flush-root"] ["Flush-Root"] (NoArg (Value "Flush-Root" "yes"))
               "Discard and recreate the clean and build environments"

flushPool :: Params -> Bool
flushPool params = values params flushPoolOpt /= []
flushPoolOpt = Param [] ["flush-pool"] ["Flush-Pool"] (NoArg (Value "Flush-Pool" "yes"))
               "Discard the packages in the local pool before building."

flushSource :: Params -> Bool
flushSource params = values params flushSourceOpt /= []
flushSourceOpt = Param [] ["flush-source"] ["Flush-Source"] (NoArg (Value "Flush-Source" "yes"))
                 "Discard and re-download all source code."

flushAll :: Params -> Bool
flushAll params = values params flushAllOpt /= []
flushAllOpt = Param [] ["flush-all"] ["Flush-All"] (NoArg (Value "Flush-All" "yes"))
              "Remove and re-create the entire autobuilder working directory."

preferred :: Params -> [String]
preferred params = values params preferOpt
preferOpt = Param [] ["prefer"] ["Prefer"] (ReqArg (Value "Prefer") "PACKAGE")
	    (text ["When selecting build dependencies, prefer this particular package",
                   "over other alternatives that could fulfill the dependency, even if",
                   "this package seems older than some other alternative.  For example,",
                   "the c-compiler virtual package is provided by gcc-3.3, gcc-3.4,",
                   "gcc-4.0, etc.  If 'Prefer: gcc-3.4' is used, a dependency on",
                   "c-compiler will choose gcc-3.4 over the others if possible."])

noVersionFix :: Params -> Bool
noVersionFix params = values params noVersionFixOpt /= []
noVersionFixOpt = Param [] ["no-version-fix"] ["No-Version-Fix"] (NoArg (Value "No-Version-Fix" "yes"))
		  (text ["Don't modify the package's version before building.  The version",
                         "number is modified before some builds to ensure newer builds",
                         "are assigned later version numbers, and to prevent collisions",
                         "upon uploading."])

createRelease :: Params -> [String]
createRelease params = values params createReleaseOpt
createReleaseOpt = Param [] ["create-release"] ["Create-Release"] (ReqArg (Value "Create-Release") "NAME") 
                   "If necessary, pass an argument to newdist to create a new release in the upload repository."

autobuilderEmail :: Params -> String
autobuilderEmail params = case nub $ values params autobuilderEmailOpt of
                            [] -> "Autobuilder Email Not Set <autobuilder@somewhere>"
                            [x] -> x
                            xs -> error $ "Multiple conflicting values for Autobuilder-Email: " ++ show xs
autobuilderEmailOpt = Param [] ["autobuilder-email"] ["Autobuilder-Email"] (ReqArg (Value "Autobuilder-Email") "EMAIL")
                      "Email address of autobuilder for use in generated changelog entries."

-- |Find the name of the base release against which we will be building.
baseRelease :: Params -> SliceName
baseRelease params =
    case (nub $ values params baseReleaseOpt) of
      [] -> error "Missing 'Base-Release' parameter"
      (x : _) -> (SliceName x)
baseReleaseOpt = Param [] ["base-release"] ["Base-Release"] (ReqArg (Value "Base-Release")  "NAME")
                 (text ["The name of the release we are basing our release on.  This sources.list",
                        "is combined with the one constructed from the Build-URI to create",
                        "the build environment."])

uploadURI :: Params -> Maybe URI
uploadURI params =
    case values params uploadURIOpt of
      [] -> Nothing
      (x : _) -> maybe (error ("Invalid Upload-URI: " ++ x)) Just (parseURI x)
uploadURIOpt = Param [] ["upload-uri"] ["Upload-URI"] (ReqArg (Value "Upload-URI") "URI")
               (text ["This URI is the address of the remote repository to which packages will be",
                      "uploaded after a run with no failures, when the --do-upload flag is used.",
                      "Packages are uploaded to the directory created by appending '/incoming'",
                      "to this URI.  This is different from the local repository, where each packages is",
                      "uploaded immediately after it is built for use as build dependnecies of other",
                      "packages during the same run."])

-- | Derived from uploadURI
uploadHost :: Params -> Maybe String
uploadHost params = maybe Nothing (Just . uriRegName) . maybe Nothing uriAuthority . uploadURI $ params

-- |Return the name of the release we will be building packages for.
buildRelease :: Params -> ReleaseName
buildRelease params =
    case values params buildReleaseOpt of
      [] -> error "Missing Build-Release parameter"
      (x : _) -> ReleaseName x
buildReleaseOpt = Param [] ["build-release"] ["Build-Release"] (ReqArg (Value "Build-Release") "NAME")
                  "The name of the release that the packages we build will be uploaded to."

style :: Params -> IOStyle -> IOStyle
style params =
    styleParams' . styleParams . defaultStyle
    where
      styleParams' = setVerbosity (verbosity params) . setPrefixes "" ""
      styleParams = foldl (.) id . map readStyle $ (values params styleOpt)
      defaultStyle = (setError (Just "failed.") .
                      setEcho False .
                      setElapsed False)
styleOpt = Param [] ["style"] ["Style"] (ReqArg (Value "Style") "STYLE SPEC")
           "Add to or change the default output style"

readStyle :: String -> IOStyle -> IOStyle
readStyle text =
    case (mapSnd tail . break (== '=')) text of
      ("Start", message) -> setStart . Just $ message
      ("Finish", message) -> setFinish . Just $ message
      ("Error", message) -> setError . Just $ message
      ("Output", "Indented") -> addPrefixes "" ""
      ("Output", "Dots") -> dotStyle IO.stdout . dotStyle IO.stderr
      ("Output", "Quiet") -> quietStyle IO.stderr . quietStyle IO.stdout
      ("Echo", flag) -> setEcho (readFlag flag)
      ("Elapsed", flag) -> setElapsed (readFlag flag)
      ("Verbosity", value) -> setVerbosity (read value)
      ("Indent", prefix) -> addPrefixes prefix prefix
      _ -> id
    where
      readFlag "yes" = True
      readFlag "no" = False
      readFlag "true" = True
      readFlag "false" = True
      readFlag text = error ("Unrecognized bool: " ++ text)

verbosity :: Params -> Int
verbosity params = foldr (+) 0 (map read (values params verbosityOpt))

verbosityOpt = Param ['v'] [] ["Verbosity"] (OptArg (\ x -> Value "Verbosity" (maybe "1" id x)) "INCREMENT")
	       "How chatty? (see also --style)"

-- | Flag: --sources, config: Sources
sources :: Params -> [String]
sources params = values params sourcesOpt

sourcesOpt = Param [] ["sources"] ["Sources"] (ReqArg (Value "Sources") "NAME LINES")
             (text ["(Config file only.)  Specify the a distribution name and, on",
                    "succeeding lines, the sources.list for that distribution."])

-- | Flag: --target, config: Targets
targets :: Params -> [String]
targets params = concat (map words (values params targetsOpt))

targetsOpt = Param ['t'] ["target", "targets"] ["Target", "Targets"] (ReqArg (Value "Targets") "TARGET TARGET...")
	     (text ["Specify one or more build targets, methods for obtaining the source",
                    "code of a package to be built.  See TARGET TYPES below for information",
                    "about the available target types." ])

-- |Return the value of the --omit-target flag.
omitTargets :: Params -> [String]
omitTargets params = concat (map words (values params omitTargetsOpt))

omitTargetsOpt = Param [] ["omit-target", "omit-targets"] ["Omit-Target", "Omit-Targets"]
                 (ReqArg (Value "Omit-Targets") "TARGET TARGET...")
                 (text ["Remove these from the list of targets given with the",
                        "--target directive.  This option is used to specify",
                        "targets which are temporarily broken in a particular",
                        "distribution."])
defaultArchitectureList :: [String]
defaultArchitectureList = ["i386", "amd64"]

archList :: Params -> [Arch]
archList params =
    map Binary (concat (map words (case values params archListOpt of
                                     [] -> defaultArchitectureList
                                     xs -> xs)))
archListOpt = Param [] ["architecture-list"] ["Architecture-List"] (ReqArg (Value "Architecture-List") "'ARCH ARCH...'")
	      (text ["The list of architectures to prepare the repository to accept.",
                     "By default this is " ++ concat (intersperse " " defaultArchitectureList)])

buildDepends :: Params -> [String]
buildDepends params = values params buildDependsOpt
buildDependsOpt = Param [] ["build-depends"] ["Build-Depends"] (ReqArg (Value "Build-Depends") "PACKAGE")
                  "Add a missing build dependency"

setEnv :: Params -> [String]
setEnv params = values params setEnvOpt
setEnvOpt = Param [] ["setenv"] ["Set-Env"] (ReqArg (Value "Set-Env") "VAR=VALUE")
            "Set an environment variable during the build"

relaxDepends :: Params -> [(String, Maybe String)]
relaxDepends params =
    map (makePair . words) (values params relaxDependsOpt)
    where
      makePair [a] = (a, Nothing)
      makePair (a : b : _) = (a, Just b)
      makePair [] = error "Invalid Relax-Depends value"
relaxDependsOpt = Param [] ["relax-depends"] ["Relax-Depends"] (ReqArg (Value "Relax-Depends") "DEPENDENCY [SOURCE]")
                  (text ["Do not trigger builds due to new versions of this package",
                         "appears, optionally specifying which source package not to build."])

text :: [String] -> String
text lines =
    -- Note that sentence ends will be followed by an empty string
    let words = splitRegex (mkRegex " ") (concat (intersperse " " lines)) in
    concat . intersperse "\n" . reverse . foldl addword [] $ words
    where
      addword :: [String] -> String -> [String]
      addword [] word = [word]
      addword (line : lines) word =
          if length (line ++ " " ++ word) < 80
          then (line ++ " " ++ word) : lines
          else (word : line : lines)

-- Constants

dirtyRoot :: Params -> EnvRoot
dirtyRoot params = dirtyRootOfRelease params (Params.buildRelease params)
    --EnvRoot $ topDir params ++ "/dists/" ++ show (buildRelease params) ++ "/build-" ++ (show (Params.strictness params))

cleanRoot :: Params -> EnvRoot
cleanRoot params = cleanRootOfRelease params (Params.buildRelease params)
    -- cleanRootOfRelease params (Params.buildRelease params)

dirtyRootOfRelease :: Params -> ReleaseName -> EnvRoot
dirtyRootOfRelease params distro =
    EnvRoot $ topDir params ++ "/dists/" ++ relName distro ++ "/build-" ++ (show (Params.strictness params))
    --ReleaseCache.dirtyRoot distro (show (Params.strictness params))

cleanRootOfRelease :: Params -> ReleaseName -> EnvRoot
cleanRootOfRelease params distro =
    EnvRoot $ topDir params ++ "/dists/" ++ relName distro ++ "/clean-" ++ (show (Params.strictness params))
    --ReleaseCache.cleanRoot distro (show (Params.strictness params))

-- |Location of the local repository for uploaded packages.
localPoolDir :: Params -> FilePath
localPoolDir params = Params.topDir params ++ "/localpools/" ++ relName (buildRelease params)
