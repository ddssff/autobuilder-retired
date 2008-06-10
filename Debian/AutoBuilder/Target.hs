{-# LANGUAGE PatternSignatures #-}
-- |A Target represents a particular set of source code and the
-- methods to retrieve and update it.
--
-- Author: David Fox <ddssff@gmail.com>
module Debian.AutoBuilder.Target
    ( Target(..)
    , targetName	-- Target -> String
    , changelogText	-- Tgt -> Maybe String -> [PkgVersion] -> String
    , prepareTarget	-- ReleaseName -> OSImage -> Int -> Int -> Tgt -> IO Target
    , readSpec		-- Bool -> FilePath -> Bool -> [DistroCache] -> String -> IO Tgt
    , buildTargets
    , showTargets 
    , targetDocumentation
    ) where

import		 Debian.Control
import qualified Debian.Control.ByteString as B
import qualified Debian.Control.String as S(fieldValue)
import qualified Debian.GenBuildDeps as GenBuildDeps
import		 Debian.Relation.ByteString as B
import		 Debian.Repo
import		 Debian.Shell
import		 Debian.Time
import		 Debian.Version
import		 Debian.VersionPolicy

import		 Control.Exception
import		 Control.Monad.RWS hiding (All)
import		 Extra.TIO
import		 Extra.Either
import		 Extra.Files
import		 Extra.List
import		 Extra.Misc
import		 Debian.AutoBuilder.BuildTarget as BuildTarget
import qualified Debian.AutoBuilder.BuildTarget.Apt as Apt
import qualified Debian.AutoBuilder.BuildTarget.Darcs as Darcs
import qualified Debian.AutoBuilder.BuildTarget.DebDir as DebDir
import qualified Debian.AutoBuilder.BuildTarget.Hg as Hg
import qualified Debian.AutoBuilder.BuildTarget.Proc as Proc
import qualified Debian.AutoBuilder.BuildTarget.Quilt as Quilt
import qualified Debian.AutoBuilder.BuildTarget.SourceDeb as SourceDeb
import qualified Debian.AutoBuilder.BuildTarget.Svn as Svn
import qualified Debian.AutoBuilder.BuildTarget.Tla as Tla
import qualified Debian.AutoBuilder.BuildTarget.Bzr as Bzr
import qualified Debian.AutoBuilder.BuildTarget.Uri as Uri
--import		 Control.Monad
import		 Control.Monad.Reader
import		 System.Unix.Process hiding (processOutput)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import		 Data.List
import qualified Data.Map as Map
import		 Data.Maybe
import qualified Data.Set as Set
import qualified Debian.AutoBuilder.Params as Params
import		 System.Directory
--import		 System.Locale
import		 System.Exit
import		 System.IO
import		 System.Posix.Files
import		 System.Time
import		 Text.Printf
import		 Text.Regex

targetDocumentation :: String
targetDocumentation =
    "TARGET TYPES\n\nEach argument to --target describes a technique for obtaining\n" ++
    "the source code used to build a target.  The following target types are available:\n\n" ++
    concat (intersperse "\n\n" $
            map (concat . intersperse "\n  ")
            [ [ "dir:<path> - A target of this form simply uses whatever it finds on"
              , "the local machine at the given path as the debian source tree."
              , "Packages built using this targets are not allowed to be uploaded"
              , "since they include no revision control information." ]
            , Apt.documentation
            , Darcs.documentation
            , DebDir.documentation
            , Hg.documentation
            , Proc.documentation
            , Quilt.documentation
            , SourceDeb.documentation
            , Svn.documentation
            , Tla.documentation
            , Uri.documentation ])

-- | Build target info.
data Target
    = Target { realSource :: Tgt		-- ^ The source code obtained from the SCCS
             , cleanSource :: DebianBuildTree	-- ^ The source code stripped of SCCS info
             , targetEntry :: ChangeLogEntry	-- ^ The contents of the most recent changelog entry
             , targetControl :: Control		-- ^ The contents of debian/control
             , targetVersion :: DebianVersion	-- ^ The version number in debian/changelog
             , targetRevision :: Maybe String	-- ^ The value computed by 'BuildTarget.revision'
             }

instance Show Target where
    show target = show . realSource $ target

-- | The /Source:/ attribute of debian\/control.
targetName :: Target -> String
targetName target =
    case targetControl target of
      Control (paragraph : _) ->
          maybe (error "Missing Source field") id $ fieldValue "Source" paragraph
      _ -> error "Target control information missing"

countAndPrepareTargets :: OSImage -> [Tgt] -> TIO [Target]
countAndPrepareTargets os targets =
    countTasks (zip (map show targets) (map (prepareTarget os) targets))

-- |Prepare a target for building in the given environment.  At this
-- point, the target needs to be a DebianSourceTree or a
-- DebianBuildTree. 
prepareTarget :: OSImage -> Tgt -> TIO Target
prepareTarget os tgt@(Tgt source) =
    do tree <- prepareBuild os source >>= 
               return . maybe (error $ "Could not find Debian build tree for " ++ show source) id
       let ctl = control tree
           latest = entry tree
           ver = logVersion latest
       rev <- BuildTarget.revision source
       return $ Target tgt tree latest ctl ver (either (const Nothing) Just rev)

-- |'prepareBuild' returns a Debian build tree for a target with all
-- the revision control files associated with the old target removed.
-- This ensures that the tarball and\/or the .diff.gz file in the deb
-- don't contain extra junk.  It also makes sure that debian\/rules is
-- executable.
prepareBuild :: (BuildTarget t) => OSImage -> t -> TIO (Maybe DebianBuildTree)
prepareBuild os target =
    do debBuild <- findOneDebianBuildTree (getTop target)
       case debBuild of
         Just tree -> copyBuild tree >>= return . either (const Nothing) Just
         Nothing ->
             do debSource <- findDebianSourceTree (getTop target)
                case debSource of
                  Left message -> vEPutStrBl 0 message >> return Nothing
                  Right tree -> copySource tree >>= return . either (const Nothing) Just
    where
      copySource :: DebianSourceTree -> TIO (Either String DebianBuildTree)
      copySource debSource =
          do let name = logPackage . entry $ debSource
                 dest = EnvPath (rootDir os) ("/work/build/" ++ name)
                 ver = Debian.Version.version . logVersion . entry $ debSource
                 newdir = escapeForBuild $ name ++ "-" ++ ver
             --io $ System.IO.hPutStrLn System.IO.stderr $ "copySource " ++ show debSource
             copy <- copyDebianSourceTree debSource (appendPath ("/" ++ newdir) dest)
             -- Clean the revision control files for this target out of the copy of the source tree
             case copy of
               Left message -> return (Left message)
               Right copy ->
                   do cleanTarget target (topdir copy)
                      findDebianBuildTree dest newdir
      copyBuild :: DebianBuildTree -> TIO (Either String DebianBuildTree)
      copyBuild debBuild =
          do let name = logPackage . entry $ debBuild
                 dest = EnvPath (rootDir os) ("/work/build/" ++ name)
                 ver = Debian.Version.version . logVersion . entry $ debBuild
                 newdir = escapeForBuild $ name ++ "-" ++ ver
             --io $ System.IO.hPutStrLn System.IO.stderr $ "copyBuild " ++ show debBuild
             copy <- copyDebianBuildTree debBuild dest
             case copy of
               Left message -> return (Left message)
               Right copy -> 
                   do cleanTarget target (topdir copy)
                      when (newdir /= (subdir debBuild))
                               (lift $ renameDirectory (outsidePath dest ++ "/" ++ subdir debBuild) (outsidePath dest ++ "/" ++ newdir))
                      findDebianBuildTree dest newdir

-- |Make a path "safe" for building.  This shouldn't be necessary,
-- but various packages make various assumptions about the type
-- of characters that appear in the name of the working directory
-- that the build is performed in.  For example, kdenetwork objects
-- to the colon, kdebase objects to the plus sign, and so on.
escapeForBuild :: FilePath -> FilePath
escapeForBuild =
    map escape
    where
      escape ':' = '_'
      escape '+' = '_'
      escape c = c

-- |Generate the details section of the package's new changelog entry
-- based on the target type and version info.  This includes the
-- revision info and build dependency versions in a human readable
-- form.  FIXME: this should also include revision control log
-- entries.
changelogText :: Tgt -> Maybe String -> [PkgVersion] -> [PkgVersion] -> String
changelogText (Tgt spec) revision oldDeps newDeps =
    ("  * " ++ logText spec revision ++ "\n" ++ depChanges changedDeps ++ "\n")
    where
      depChanges [] = ""
      depChanges _ = "  * Build dependency changes:" ++ prefix ++ consperse prefix padded ++ "\n"
      padded = map concat . columns . map showDepChange $ changedDeps
      changedDeps = Set.toList (Set.difference (Set.fromList newDeps) (Set.fromList oldDeps))
      showDepChange newDep =
          case filter (hasName (getName newDep)) oldDeps of
            [] -> [" " ++ getName newDep ++ ": ", "(none)", " -> ", show (getVersion newDep)]
            (oldDep : _) -> [" " ++ getName newDep ++ ": ", show (getVersion oldDep), " -> ", show (getVersion newDep)]
      hasName name deps = ((== name) . getName) deps
      prefix = "\n    "

-- |Generate the string of build dependency versions:
-- package1=version1 package2=version2 ...
_formatVersions :: [PkgVersion] -> String
_formatVersions buildDeps =
    -- "\n  * Build dependency versions:" ++
    prefix ++
    consperse prefix (map show buildDeps) ++
    "\n"
    where prefix = "\n    "

readSpec :: Bool -> FilePath -> Bool -> SourcesChangedAction -> [NamedSliceList] -> String -> AptIO (Either String Tgt)
readSpec debug top flush ifSourcesChanged distros text =
    tio (vEPutStrBl 0 (text ++ ":")) >> mapRWST (local (appPrefix " ")) readSpec'
    where
      readSpec' =
          case text of
            'a':'p':'t':':' : target -> Apt.prepareApt top flush ifSourcesChanged distros target
            'd':'a':'r':'c':'s':':' : target -> tio $ Darcs.prepareDarcs debug top flush target
            'd':'e':'b':'-':'d':'i':'r':':' : target ->
                do pair <- parsePair debug target
                   case pair of
                     Left message -> return (Left message)
                     Right (upstream, debian) -> tio $ DebDir.prepareDebDir debug top flush upstream debian
            'd':'i':'r':':' : target -> tio $ prepareDir debug top flush (rootEnvPath target)
            'h':'g':':' : target -> tio $ Hg.prepareHg debug top flush target
            'q':'u':'i':'l':'t':':' : target ->
                do pair <- parsePair debug target
                   case pair of
                     Left message -> return (Left message)
                     Right (base, patch) -> tio $ Quilt.prepareQuilt top flush base patch
            's':'o':'u':'r':'c':'e':'d':'e':'b':':' : target ->
                readSpec debug top flush ifSourcesChanged distros target >>=
                tio . either (return . Left . ((text ++ ": ") ++)) SourceDeb.prepareSourceDeb
            's':'v':'n':':' : target -> tio $ Svn.prepareSvn debug top flush target
            't':'l':'a':':' : target -> tio $ Tla.prepareTla top flush target
            'b':'z':'r':':' : target -> tio $ Bzr.prepareBzr top flush target
            'u':'r':'i':':' : target -> tio $ Uri.prepareUri debug top flush target
            'p':'r':'o':'c':':' : target ->
                readSpec debug top flush ifSourcesChanged distros target >>=
                tio . either (return . Left) (Proc.prepareProc top flush)
            _ -> error ("Error in target specification: " ++ text)
      parsePair :: Bool -> String -> AptIO (Either String (Tgt, Tgt))
      parsePair debug text =
          case match "\\(([^)]*)\\):\\(([^)]*)\\)" text of
            Just [baseName, patchName] ->
                do a <- readSpec debug top flush ifSourcesChanged distros baseName
                   b <- readSpec debug top flush ifSourcesChanged distros patchName
                   return (case (a, b) of
                             (Right a', Right b') -> Right (a', b')
                             (Left message, _) -> Left message
                             (_, Left message) -> Left message)
            _ -> return (Left ("Invalid spec name: " ++ text))
      match = matchRegex . mkRegex

-- | Build a set of targets.  When a target build is successful it
-- is uploaded to the incoming directory of the local repository,
-- and then the function to process the incoming queue is called.
buildTargets :: (AptCache t) => Params.Params -> OSImage -> Relations -> LocalRepository -> t -> [Tgt] -> AptIO (LocalRepository, [Target])
buildTargets _ _ _ localRepo _ [] = return (localRepo, [])
buildTargets params cleanOS globalBuildDeps localRepo poolOS targetSpecs =
    do
      -- showTargets targetSpecs
      targetList <- tio $ prepareAllTargetSource cleanOS
      tio (vEPutStrBl 0 "Building all targets:")
      failed <- {- setStyle (addPrefix " ") $ -} buildLoop cleanOS globalBuildDeps (length targetList) (targetList, [])
      return (localRepo, failed)
      --buildAll cleanOS targetList globalBuildDeps
    where
      -- Retrieve and/or update the source code of all the targets before building.
      --prepareAllTargetSource :: OSImage -> TIO [Target]
      prepareAllTargetSource cleanOS =
          do
            vEPutStrBl 0 "Assembling clean source tree for each target:"
            ({- debugStyle . -} iStyle) $ countAndPrepareTargets cleanOS targetSpecs
          where
            iStyle = setStyle (appPrefix " ")
      -- Execute the target build loop until everything is built
      --buildLoop :: OSImage -> Relations -> Int -> ([Target], [Target]) -> AptIO [Target]
      buildLoop _ _ _ ([], failed) = return failed
      buildLoop cleanOS globalBuildDeps count (unbuilt, failed) =
          do
            targetGroups <- tio $ chooseNextTarget globalBuildDeps (Params.relaxDepends params) unbuilt
            --tio (vEPutStrBl 0 ("\n\n" ++ makeTable targetGroups ++ "\n"))
            case targetGroups of
              (target, blocked, other) ->
                  tio (vEPutStrBl 0 (printf "[%2d of %2d] TARGET: %s\n"
                                     (count - length unbuilt + 1) count (show target))) >>
                  -- mapRWST (local (appPrefix " ")) (buildTarget' target) >>=
                  buildTarget' target >>=
                  either (\ e -> do tio $ vEPutStrBl 0 ("Package build failed: " ++ e)
                                    tio $ vEPutStrBl 0 ("Discarding " ++ show target ++ " and its dependencies:\n  " ++
                                                        concat (intersperse "\n  " (map show blocked)))
                                    buildLoop cleanOS globalBuildDeps count (other, (target : blocked) ++ failed))
                         (\ _ -> do cleanOS' <- updateEnv cleanOS
                                    case cleanOS' of
                                      Left e -> error ("Failed to update clean OS: " ++ e)
                                      Right cleanOS'' ->
                                          buildLoop cleanOS'' globalBuildDeps count (blocked ++ other, failed))
          where
{-
            makeTable groups =
                unlines . map (consperse " ") . columns $ ["Ready", "Blocked"] : ["-----", "-------"] : map makeRow groups
            makeRow :: [Target] -> [String]
            makeRow group =
                [(targetName . head $ group), (consperse " " . map targetNameAndDeps . tail $ group)]
                where targetNameAndDeps x = targetName x ++ " (" ++ intercalate " " (map (show . depends x) group) ++ ")"
            depends (_, depInfo1) (_, depInfo2) = GenBuildDeps.compareSource depInfo1 depInfo2
-}
            buildTarget' target =
                do tio (vBOL 0)
                   {- showElapsed "Total elapsed for target: " $ -} 
                   buildTarget params cleanOS globalBuildDeps localRepo poolOS target
      -- Find the sources.list for the distribution we will be building in.
      --indent s = setStyle (addPrefix stderr s)
      --debugStyle = setStyle (cond Debian.IO.dryRun Debian.IO.realRun (Params.debug params))

-- |Compute the list of targets that are ready to build from the build
-- dependency relations.  The return value is a list of target lists,
-- where the first element of each list is ready to build, and the
-- other elements are blocked by the first target.
--
-- It is not possible to precompute a build order for all the targets,
-- because we don't know ahead of time exactly what packages will be
-- built or even what their versions will be.  Furthermore, it is not
-- even desirable to do it that way, because then when we are actually
-- building the packages and one fails we no longer have enough
-- information to decide which other packages might still be
-- buildable.
--
-- Therefore, the algorithm we want to use is one where we look at
-- set of targets, choose one that can be built, build it, remove it
-- from the target set, and repeat until all targets are built.  We
-- can build a graph of the "has build dependency" relation and find
-- any node that has no inbound arcs and (maybe) build that.
chooseNextTarget :: Relations -> [(String, Maybe String)] -> [Target] -> TIO (Target, [Target], [Target])
chooseNextTarget globalBuildDeps relaxed targets =
    getDependencyInfo targets >>=
    -- Compute the list of build dependency groups, each of which
    -- starts with a target that is ready to build followed by
    -- targets which are blocked by the first target.
    return . GenBuildDeps.buildable depends >>=
    either (error . show . map (\ (pkg, deps) -> (targetName (fst pkg), map (targetName . fst) deps)))
	   (\ (ready, blocked, other) -> (vEPutStrBl 0 (makeTable (ready, blocked, other)) >> return (ready, blocked, other))) >>=
    (\ (ready, blocked, other) -> return (fst ready, map fst blocked, map fst other))
    where
      -- retrieve the dependency information for each target
      getDependencyInfo :: [Target] -> TIO [(Target, GenBuildDeps.DepInfo)]
      getDependencyInfo targets =
          mapM (getDepInfo globalBuildDeps . cleanSource) targets >>=
               return . zip targets >>=
               printErrors >>=
               return . filter (\ pair -> isRight (snd pair)) >>=
               return . catMaybes . map (\ (target, info) -> case info of
                                                               Left _ -> Nothing
                                                               Right info -> Just (target, info))
      printErrors depInfo =
          do case filter (either (const True) (const False) . snd) depInfo of
               [] -> return ()
               bad -> (vEPutStrBl 0
                       ("Unable to retrieve build dependency info for some targets:\n  " ++
                        concat (intersperse "\n  " (map (\ (target, error) -> show target ++ ": " ++ either id (const "") error) bad))))
             return depInfo
      getDepInfo :: Relations -> DebianBuildTree -> TIO (Either String GenBuildDeps.DepInfo)
      getDepInfo globalBuildDeps buildTree =
          do --let sourceTree = debTree buildTree
             let controlPath = appendPath "/debian/control" (debdir buildTree)
             info <- lift $ parseControlFromFile (outsidePath controlPath) >>= return . either (Left . show) (GenBuildDeps.genDep relaxed)
             -- My.ePutStr ("getDepInfo " ++ show target ++ ": " ++ show info)
             return $ either Left (Right . addRelations globalBuildDeps) info
      addRelations :: Relations -> GenBuildDeps.DepInfo -> GenBuildDeps.DepInfo
      addRelations moreRels (name, relations, bins) = (name, relations ++ moreRels, bins)
      makeTable :: ((Target, GenBuildDeps.DepInfo), [(Target, GenBuildDeps.DepInfo)], [(Target, GenBuildDeps.DepInfo)]) -> String
      makeTable (target, blocked, other) =
          unlines . map (consperse " ") . columns $ [["Ready:", targetName (fst target)],
                                                     ["Blocked:", (intercalate " " . map (targetName . fst) $ blocked)],
                                                     ["Other:", (intercalate " " . map (targetName . fst) $ other)]]
{-
      makeRow :: (Target, GenBuildDeps.DepInfo) -> [(Target, GenBuildDeps.DepInfo)] -> [String]
      makeRow ready blocked =
          [(targetName . fst $ ready), (intercalate " " . map (targetName . fst) $ blocked)]
          --[(targetNameAndDeps . head $ group), (intercalate " " . map targetNameAndDeps . tail $ group)]
          where
            targetNameAndDeps p@(t, _) = targetName t ++ " (" ++ targetDeps p ++ ")"
            targetDeps p =
                intercalate " " (catMaybes (map (\ p'@(t', _) ->
                                                 case depends p p' of 
                                                   EQ -> Nothing
                                                   x -> Just (show x ++ " " ++ targetName t')) group))
-}
      depends (_, depInfo1) (_, depInfo2) = GenBuildDeps.compareSource depInfo1 depInfo2
--showTargets :: Show a => [a] -> IO ()
showTargets targets =
    vEPutStrBl 0 ("\n" ++ 
                  unlines
	          (heading :
                   map (const '-') heading :
                   map (\ (n, name) -> printf "%4d. %s" n name) pairs) ++
                  "\n")
    where
      (pairs :: [(Int, String)]) = zip [1..] targets
      heading = show (length targets) ++ " Targets:"

-- |Represents a decision whether to build a package, with a text juststification.
data BuildDecision
    = Yes String
    | No String
    | Arch String	-- Needs a -B build, architecture dependent files only
    | Auto String	-- Needs a 'automated' rebuild, with a generated version number and log entry
    | Error String	-- A fatal condition was encountered - e.g. a build dependency became older since last build

instance Show BuildDecision where
    show (Yes reason) = "Yes - " ++ reason
    show (No reason) = "No - " ++ reason
    show (Arch reason) = "Yes - " ++ reason
    show (Auto reason) = "Yes - " ++ reason
    show (Error reason) = "Error - " ++ reason

-- Decide whether a target needs to be built and, if so, build it.
buildTarget ::
    (AptCache t) =>
    Params.Params ->			-- configuration info
    OSImage ->				-- cleanOS
    Relations ->			-- The build-essential relations
    LocalRepository ->			-- The local repository the packages will be uploaded to
    t ->
    Target ->				-- what to build.  The first element is the original target,
					--   the second is the same with any source code control
					--   files cleaned out (so the .diff.gz will look good.)
    AptIO (Either String LocalRepository)	-- The local repository after the upload, or an error message
buildTarget params cleanOS globalBuildDeps repo poolOS target =
    do
      tio (syncPool cleanOS)
      -- Get the control file from the clean source and compute the
      -- build dependencies
      let debianControl = targetControl target
      tio (vEPutStrBl 1 "Loading package lists and searching for build dependency solution...")
      solutions <- tio (iStyle $ buildDepSolutions' (Params.preferred params) cleanOS globalBuildDeps debianControl)
      case solutions of
        Left excuse -> do tio (vEPutStrBl 0 ("Couldn't satisfy build dependencies\n " ++ excuse))
                          return $ Left ("Couldn't satisfy build dependencies\n" ++ excuse)
        Right [] -> error "Internal error 4"
        Right ((count, sourceDependencies) : _) ->
            do let sourceDependencies' = map makeVersion sourceDependencies
               tio (vEPutStrBl 2 ("Using build dependency solution #" ++ show count))
               tio (vEPutStr 3 (concat (map (("\n  " ++) . show) sourceDependencies')))
               -- Get the newest available version of a source package,
               -- along with its status, either Indep or All
               let (releaseControlInfo, releaseStatus, message) = getReleaseControlInfo cleanOS packageName
               tio (vEPutStrBl 2 message)
               tio (vEPutStrBl 2 ("Status of " ++ packageName ++ maybe "" (\ p -> "-" ++ show (packageVersion . sourcePackageID $ p)) releaseControlInfo ++
                             ": " ++ explainSourcePackageStatus releaseStatus))
               --My.ePutStr ("Target control info:\n" ++ show releaseControlInfo)
               -- Get the revision info of the package currently in the dist
               let oldVersion = maybe Nothing (Just . getOldVersion) releaseControlInfo
               let (oldRevision, oldSrcVersion, oldDependencies) = maybe (Nothing, Nothing, []) getOldRevision releaseControlInfo
               -- Compute the Revision: string for the source tree.  This
               -- string will appear in the .dsc file for the package, and will
               -- then be copied into the Sources.gz file of the distribution.
               -- For a TLA target this is the current revision name, by
               -- default it is simply the debian version number.  The version
               -- number in the source tree should not have our vendor tag,
               -- that should only be added by the autobuilder.
               sourceRevision <- case realSource target of (Tgt spec) -> tio (BuildTarget.revision spec) >>= return . either (const Nothing) Just
               -- Get the changelog entry from the clean source
               let sourceLog = entry . cleanSource $ target
               let sourceVersion = logVersion sourceLog
               tio (vEPutStrBl 1 ("Released source version: " ++ show oldSrcVersion))
               tio (vEPutStrBl 1 ("Released version: " ++ show oldVersion))
               tio (vEPutStrBl 1 ("Current source version: " ++ show sourceVersion))
               let sourcePackages = aptSourcePackagesSorted poolOS [packageName]
               let newVersion = computeNewVersion params sourcePackages releaseControlInfo sourceVersion
               let ignoredBuildDeps = filterPairs (logPackage sourceLog) (Params.relaxDepends params)
               let decision =
                       buildDecision (realSource target) (Params.vendorTag params)
                                         (Params.forceBuild params) ignoredBuildDeps sourceLog
                                         oldVersion oldSrcVersion oldRevision oldDependencies releaseStatus
                                         sourceVersion sourceRevision sourceDependencies'
               tio (vEPutStrBl 0 ("Build decision: " ++ show decision))
               -- FIXME: incorporate the release status into the build decision
               case newVersion of
                 Left message ->
                    return (Left message)
                 Right version ->
                    case decision of
                      Error message -> return (Left message)
                      No _ -> return (Right repo)
                      Yes _ ->  buildPackage params cleanOS (Just version) oldDependencies sourceRevision sourceDependencies' target None repo sourceLog
                      Arch _ -> buildPackage params cleanOS oldVersion oldDependencies sourceRevision sourceDependencies' target releaseStatus repo sourceLog
                      Auto _ -> buildPackage params cleanOS (Just version) oldDependencies sourceRevision sourceDependencies' target None repo sourceLog
    where
      --buildTree = maybe (error $ "Invalid target for build: " ++ show target) id (getBuildTree . cleanSource $ target)
      packageName = targetName target
      -- Find or create an apt-get environment that will see all the packages
      -- in both the upload repository and the local repository, and then use
      -- it to compute a list of all existing versions of the package.
{-    compareVersion a b = case (fieldValue "Version" a, fieldValue "Version" b) of
                             (Just a', Just b') -> compare (parseDebianVersion a') (parseDebianVersion b')
                             _ -> error "Missing Version field" -}
      filterPairs :: String -> [(String, Maybe String)] -> [String]
      filterPairs name = catMaybes . map (filterPair name)
      filterPair _ (x, Nothing) = Just x			-- Always omit this dependency
      filterPair name (x, Just y) | (y == name) = Just x	-- Omit a dependency on a particular package
      filterPair _ (_, Just _) = Nothing			-- Omit if the package name doesn't match
      iStyle = setStyle (appPrefix " ")

-- |Convert to a simple name and version record to interface with older
-- code.
makeVersion :: BinaryPackage -> PkgVersion
makeVersion package =
    PkgVersion { getName = packageName (packageID package)
               , getVersion = packageVersion (packageID package) }

-- | Build a package and upload it to the local repository.
buildPackage :: Params.Params -> OSImage -> Maybe DebianVersion -> [PkgVersion] -> Maybe String -> [PkgVersion] -> Target -> SourcePackageStatus -> LocalRepository -> ChangeLogEntry -> AptIO (Either String LocalRepository)
buildPackage params cleanOS newVersion oldDependencies sourceRevision sourceDependencies target status repo sourceLog =
    checkDryRun >>
    (tio prepareImage) >>=
    either (return . Left) (tio . logEntry) >>=
    either (return . Left) (tio . build) >>=
    either (return . Left) (tio . find) >>=
    either (return . Left) upload
    where
      checkDryRun = when (Params.dryRun params)  (do tio (vEPutStrBl 0 "Not proceeding due to -n option.")
                                                     io (exitWith ExitSuccess))
      prepareImage = prepareBuildImage params cleanOS sourceDependencies buildOS target (Params.strictness params)
      logEntry :: DebianBuildTree -> TIO (Either String DebianBuildTree)
      logEntry buildTree = 
          case Params.noClean params of
            False -> lift $ maybeAddLogEntry buildTree newVersion >> return (Right buildTree)
            True -> return (Right buildTree)
      build :: DebianBuildTree -> TIO (Either String (DebianBuildTree, TimeDiff))
      build buildTree =
          case realSource target of
            Tgt t -> do result <- buildPkg (Params.noClean params) (Params.setEnv params) buildOS buildTree status t
                        case result of
                          Left message -> return (Left message)
                          Right elapsed -> return (Right (buildTree, elapsed))
      find (buildTree, elapsed) =
          lift $ findChanges buildTree >>= return . either Left (\ changesFile -> Right (changesFile, elapsed))
      upload :: (ChangesFile, TimeDiff) -> AptIO (Either String LocalRepository)
      upload (changesFile, elapsed) = doLocalUpload elapsed changesFile
      -- Depending on the strictness, build dependencies either
      -- get installed into the clean or the build environment.
      maybeAddLogEntry _ Nothing = return ()
      maybeAddLogEntry buildTree (Just newVersion) = makeLogEntry newVersion >>= (flip addLogEntry) buildTree
      makeLogEntry newVersion = 
          do
            date <- getCurrentLocalRFC822Time
            return $ sourceLog {logVersion=newVersion,
                                logDists=[Params.buildRelease params],
                                logWho=Params.autobuilderEmail params,
                                logDate=date,
                                logComments=
                                         ("  * Automatic build due to dependency changes.\n" ++
                                          changelogText (realSource target) sourceRevision oldDependencies sourceDependencies)}
      setDistribution name changes =
          let (Paragraph fields) = changeInfo changes in
          let info' = map (setDist name) fields in
          changes { changeInfo = Paragraph info'
                  , changeRelease = name }
          where setDist name (Field ("Distribution", _)) = Field ("Distribution", ' ' : releaseName' name)
                setDist _ other = other
      doLocalUpload :: TimeDiff -> ChangesFile -> AptIO (Either String LocalRepository)
      doLocalUpload elapsed changesFile =
          do
            (changesFile' :: ChangesFile) <-
		-- Set the Distribution field in the .changes file to the one
                -- specified by the autobuilder Build-Release parameter.
                return (setDistribution (Params.buildRelease params) changesFile) >>=
                -- Insert information about the build into the .changes file.
                io . updateChangesFile elapsed >>=
                -- Insert the revision info into the .dsc file and update
                -- the md5sum of the .dsc file in the .changes file.
                io . setRevisionInfo (logVersion sourceLog) sourceRevision sourceDependencies
            -- Upload to the local apt repository
            tio (uploadLocal repo changesFile')
            -- The upload to the local repository is done even when
            -- the --dry-run flag is given.  Or it would be if we
            -- didn't exit when the first buildworthy target is found.
            (_, errors) <- scanIncoming True Nothing repo
            case errors of
              [] -> return . Right $ repo
              _ -> return . Left $ "Local upload failed:\n" ++ showErrors (map snd errors)
      buildOS = Debian.Repo.chrootEnv cleanOS (Params.dirtyRoot params)

-- |Prepare the build image by copying the clean image, installing
-- dependencies, and copying the clean source tree.  For a lax build
-- these operations take place in a different order from other types
-- of builds.  For lax: dependencies, then image copy, then source
-- copy.  For other: image copy, then source copy, then dependencies.
prepareBuildImage :: Params.Params -> OSImage -> [PkgVersion] -> OSImage -> Target -> Params.Strictness -> TIO (Either String DebianBuildTree)
prepareBuildImage params cleanOS sourceDependencies buildOS target@(Target (Tgt tgt) _ _ _ _ _) Params.Lax =
    -- Install dependencies directly into the clean environment
    installDependencies cleanOS (cleanSource target) buildDepends sourceDependencies >>=
    either (return . Left) (prepareTree noClean)
    where
      prepareTree True _ =
          findOneDebianBuildTree newPath >>=
          return . maybe (Left $ "No build tree at " ++ show newPath) Right 
      prepareTree False _ =
          vBOL 0 >>
          vEPutStr 1 "Syncing buildOS" >>
          Debian.Repo.syncEnv cleanOS buildOS >>=
          const (prepareCopy tgt (cleanSource target) newPath)
      buildDepends = (Params.buildDepends params)
      noClean = Params.noClean params
      newPath = EnvPath {envRoot = rootDir buildOS, envPath = envPath oldPath}
      oldPath = topdir . cleanSource $ target
prepareBuildImage params cleanOS sourceDependencies buildOS target@(Target (Tgt tgt) _ _ _ _ _) _ =
    -- Install dependencies directly into the build environment
    findTree noClean >>=
    either (return . Left) downloadDeps >>=
    either (return . Left) (syncEnv noClean) >>=
    either (return . Left) installDeps
    where
      findTree :: Bool -> TIO (Either String DebianBuildTree)
      findTree False = prepareCopy tgt (cleanSource target) newPath
      findTree True = findOneDebianBuildTree newPath >>= return . maybe (Left "build tree not found") Right
      downloadDeps :: DebianBuildTree -> TIO (Either String DebianBuildTree)
      downloadDeps buildTree = iStyle (downloadDependencies cleanOS buildTree buildDepends sourceDependencies) >>=
                               either (return . Left) (const (return (Right buildTree)))
      syncEnv :: Bool -> DebianBuildTree -> TIO (Either String (OSImage, DebianBuildTree))
      syncEnv False buildTree = vEPutStrBl 1 "Syncing buildOS" >>
                                Debian.Repo.syncEnv cleanOS buildOS >>= (\ os -> return (Right (os, buildTree)))
      syncEnv True buildTree = return (Right (buildOS, buildTree))
      installDeps :: (OSImage, DebianBuildTree) -> TIO (Either String DebianBuildTree)
      installDeps (buildOS, buildTree) = iStyle (installDependencies buildOS buildTree buildDepends sourceDependencies) >>=
                                         either (return . Left) (const (return (Right buildTree)))
{-
    do
      buildTree <- 
          case noClean of
            False -> prepareCopy tgt (cleanSource target) newPath
            True -> findOneDebianBuildTree newPath >>= return . maybe (Left "build tree not found") Right
      case buildTree of
        Left message -> return (Left message)
        Right buildTree -> iStyle $ downloadDependencies cleanOS buildTree buildDepends sourceDependencies
      case noClean of
        False -> vEPutStrBl 1 "Syncing buildOS" >> Debian.Repo.syncEnv cleanOS buildOS
        True -> return buildOS
      case buildTree of
        Left message -> return (Left message)
        Right buildTree -> iStyle $ installDependencies buildOS buildTree buildDepends sourceDependencies
      return buildTree
    where
-}
      buildDepends = Params.buildDepends params
      noClean = Params.noClean params
      newPath = EnvPath {envRoot = rootDir buildOS, envPath = (envPath . topdir . cleanSource $ target)}
      iStyle = setStyle (appPrefix " ")
            
-- | Get the control info for the newest version of a source package
-- available in a release.  Make sure that the files for this build
-- architecture are available.
getReleaseControlInfo :: OSImage -> String -> (Maybe SourcePackage, SourcePackageStatus, String)
getReleaseControlInfo cleanOS packageName =
    case zip sourcePackages (map (isComplete binaryPackages) sourcePackagesWithBinaryNames) of
      (info, True) : _ -> (Just info, All, message)
      (info, False) : _ -> (Just info, Indep, message)
      _ -> (Nothing, None, message)
    where
{-
      message =
          let results = (map (isComplete binaryPackages) sourcePackagesWithBinaryNames) in
          concat . intersperse "\n" $
                     (map (\ ((sp, bn), flag) ->
                               "  isComplete " ++ show (map binaryPackageVersion binaryPackages) ++ " " ++
                               show (sourcePackageVersion sp) ++ " " ++ show bn ++ " -> " ++
                               show flag ++ "\n" ++
                               "  availableDebNames " ++ show (map binaryPackageVersion binaryPackages) ++ " " ++
                               show (sourcePackageVersion sp) ++ " -> " ++ show (availableDebNames binaryPackages sp)
                          )
                      (zip sourcePackagesWithBinaryNames results))
-}
      message =
          concat
          . intersperse "\n"
                $ (["  Source Package Versions: " ++ show (map sourcePackageVersion sourcePackages),
                    "  Required Binary Package Names:"] ++
                   map (("   " ++) . show) (zip (map sourcePackageVersion sourcePackages) (map sourcePackageBinaryNames sourcePackages)) ++
                   ["  Binary Package Versions: " ++ show (map binaryPackageVersion binaryPackages),
                    "  Available Binary Packages of Source Package:"] ++
                   map (("   " ++) . show) (zip (map sourcePackageVersion sourcePackages) (map (availableDebNames binaryPackages) sourcePackages)))
      sourcePackagesWithBinaryNames = zip sourcePackages (map sourcePackageBinaryNames sourcePackages)
      binaryPackages = Debian.Repo.binaryPackages cleanOS (nub . concat . map sourcePackageBinaryNames $ sourcePackages)
      sourcePackages = sortBy compareVersion . Debian.Repo.sourcePackages cleanOS $ [packageName]
      sourcePackageVersion package =
          case ((fieldValue "Package" . sourceParagraph $ package), (fieldValue "Version" . sourceParagraph $ package)) of
            (Just name, Just version) -> (B.unpack name, parseDebianVersion (B.unpack version))
            _ -> error "Missing Package or Version field"
      binaryPackageVersion package =
          case ((fieldValue "Package" . packageInfo $ package), (fieldValue "Version" . packageInfo $ package)) of
            (Just name, Just version) -> (B.unpack name, parseDebianVersion (B.unpack version))
            _ -> error "Missing Package or Version field"
      compareVersion a b = case ((fieldValue "Version" . sourceParagraph $ a), (fieldValue "Version" . sourceParagraph $ b)) of
                             (Just a', Just b') -> compare (parseDebianVersion . B.unpack $ b') (parseDebianVersion . B.unpack $ a')
                             _ -> error "Missing Version field"
      -- The source package is complete if the correct versions of the
      -- required binary packages are all available, either as debs or
      -- udebs.  Because it is easier to check for available debs, we
      -- do that first and only check for udebs if some names are missing.
      isComplete :: [BinaryPackage] -> (SourcePackage, [String]) -> Bool
      isComplete binaryPackages (sourcePackage, requiredBinaryNames) =
          Set.difference required availableDebs == Set.empty
                 && (unableToCheckUDebs
                        || Set.difference required (Set.union availableDebs availableUDebs) == Set.empty)
          where
            required = Set.fromList requiredBinaryNames
            -- Which binary packages produced from this source package are available?
            availableDebs = Set.fromList (availableDebNames binaryPackages sourcePackage)
            availableUDebs = Set.fromList (availableUDebNames sourcePackage)
      -- A binary package is available either if it appears in the
      -- package index, or if it is an available udeb.
      availableDebNames :: [BinaryPackage] -> SourcePackage -> [String]
      availableDebNames binaryPackages sourcePackage =
          map fst . map binaryPackageVersion . filter checkSourceVersion $ binaryPackages
          where checkSourceVersion binaryPackage = maybe False ((==) sourceVersion) (binaryPackageSourceVersion binaryPackage)
                sourceVersion = sourcePackageVersion sourcePackage
      --  or (if it is a udeb) if it simply exists on the
      -- server and has the correct filename.  There is no way to
      -- decide whether a package is a udeb from the package indexes.
      unableToCheckUDebs = True
      availableUDebNames :: SourcePackage -> [String]
      availableUDebNames _sourcePackage = undefined

-- | Get the "old" package version number, the one that was already
-- built and uploaded to the repository.
getOldVersion :: SourcePackage -> DebianVersion
getOldVersion package = packageVersion . sourcePackageID $ package

getOldRevision :: SourcePackage -> (Maybe String, Maybe DebianVersion, [PkgVersion])
getOldRevision package = 
    maybe (Nothing, Nothing, []) (parseRevision . B.unpack) (S.fieldValue "Revision" . sourceParagraph $ package)
    where
      parseRevision s =
          case words s of
            [] -> (Nothing, Nothing, [])
            (revision : sourceVersion : buildDeps)
                | not (elem '=' sourceVersion) ->
                    (Just revision, Just (parseDebianVersion sourceVersion), map readPkgVersion buildDeps)
            (revision : buildDeps) -> (Just revision, Nothing, map readPkgVersion buildDeps)

-- |Compute a new version number for a package by adding a vendor tag
-- with a number sufficiently high to trump the newest version in the
-- dist, and distinct from versions in any other dist.
computeNewVersion :: Params.Params -> [SourcePackage] -> Maybe SourcePackage -> DebianVersion -> Either String DebianVersion
computeNewVersion params
                  available		-- All the versions that exist in the pool in any dist,
					-- the new version number must not equal any of these.
                  current		-- The control file paragraph for the currently uploaded
                                        -- version in this dist.  The new version must be newer
                                        -- than this.
                  sourceVersion =	-- Version number in the changelog entry of the checked-out
                                        -- source code.  The new version must also be newer than this.
    case Params.doNotChangeVersion params of
      True -> Right sourceVersion
      False ->
          let vendor = Params.vendorTag params
              release = if (Params.isDevelopmentRelease params) then
                            Nothing else
                            (Just (sliceName (Params.baseRelease params)))
              extra = Params.extraReleaseTag params 
              aliases = Params.releaseAliases params in
          setTag aliases vendor release extra currentVersion (catMaybes . map getVersion $ available) sourceVersion
    where
      getVersion paragraph =
          maybe Nothing (Just . parseDebianVersion . B.unpack) (fieldValue "Version" . sourceParagraph $ paragraph)
      currentVersion =
          maybe Nothing (Just . parseDebianVersion . B.unpack) (maybe Nothing (fieldValue "Version" . sourceParagraph) current)

-- FIXME: Most of this code should move into Debian.Repo.Dependencies
buildDepSolutions' :: [String] -> OSImage -> Relations -> Control -> TIO (Either String [(Int, [BinaryPackage])])
buildDepSolutions' preferred os globalBuildDeps debianControl =
    do
      arch <- lift $ buildArchOfEnv (rootDir os)
      -- We don't discard any dependencies here even if they are
      -- mentioned in Relax-Depends, that only applies to deciding
      -- whether to build, once we are building we need to install all
      -- the dependencies.  Hence this empty list.
      case GenBuildDeps.buildDependencies [] debianControl of
        Left message -> return (Left message)
        Right (_, relations, _) ->
            do let relations' = relations ++ globalBuildDeps
               let relations'' = simplifyRelations packages relations' preferred arch
               -- Do not stare directly into the solutions!  Your head will
               -- explode (because there may be a lot of them.)
               case Debian.Repo.solutions packages relations'' 100000 of
                 Left error -> message 0 relations' relations'' >> return (Left error)
                 Right solutions -> message 2 relations' relations'' >> return (Right solutions)
    where
      packages = aptBinaryPackages os
      message n relations' relations'' =
          vEPutStrBl n ("Build dependency relations:\n " ++
                        concat (intersperse "\n " (map (\ (a, b) -> show a ++ " -> " ++ show b)
                                                   (zip relations' relations''))))
      -- Group and merge the relations by package.  This can only be done
      -- to AND relations that include a single OR element, but these are
      -- extremely common.  (Not yet implemented.)
{-
      mergeRelations :: Relations -> Relations
      mergeRelations relations = relations
          let pairs = zip (map namesOf relations) relations in
          let pairs' = sortBy ((==) . fst) pairs in
          let pairs'' = groupBy (\ a b -> fst a == fst b) pairs' in
          map concat (map merge pairs'')
          where
            merge ([name], rels) = ([name], [rel])
            mergeAnds ([name], (Rel _ (Just v1) a) : (Rel _ Nothing _) : more) = merge ([name], (Rel _ (Just v1) a) : more)
            mergeAnds ([name], (Rel _ Nothing a) : (Rel _ Nothing _) : more) = merge ([name], (Rel _ Nothing a) : more)
          let (simple, compound) = partition ((== 1) . length . fst) pairs in
          
          let (simple, compound) = partition ((== 1) . length . nub . map nameOf) relations in
          undefined
          where
            namesOf relation = nub (map nameOf)
            nameOf (Rel name _ _) = name
-}

parseProcCpuinfo :: IO [(String, String)]
parseProcCpuinfo =
    readFile "/proc/cpuinfo" >>= return . map makePair . catMaybes . map (matchRegex re) . lines
    where
      re = mkRegex "^(.*[^ \t])[ \t]*:[ \t]*([^ \t].*)$"
      makePair [a, b] = (a, b)
      makePair _ = error "error parsing /proc/cpuinfo"

parseProcMeminfo :: IO [(String, String)]
parseProcMeminfo =
    readFile "/proc/meminfo" >>= return . map makePair . catMaybes . map (matchRegex re) . lines
    where
      re = mkRegex "^(.*[^ \t])[ \t]*:[ \t]*([^ \t].*)$"
      makePair [a, b] = (a, b)
      makePair _ = error "error parsing /proc/meminfo"

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll _ [] = []
lookupAll a ((a', b) : pairs) | a == a' = b : (lookupAll a pairs)
lookupAll a (_ : pairs) = lookupAll a pairs

-- |Add Build-Info field to the .changes file
updateChangesFile :: TimeDiff -> ChangesFile -> IO ChangesFile
updateChangesFile elapsed changes =
    do
      let (Paragraph fields) = changeInfo changes
      -- changes <- parseControlFromFile path >>= return . either (\ _ -> error ("Couldn't parse changes files " ++ path)) id
      autobuilderVersion <- processOutput "dpkg -s autobuilder | sed -n 's/^Version: //p'" >>=
                            return . either (const Nothing) Just >>=
                            return . maybe Nothing (listToMaybe . lines)
      hostname <- processOutput "hostname" >>= either (\ _ -> return Nothing) (return . listToMaybe . lines)
      cpuInfo <- parseProcCpuinfo
      memInfo <- parseProcMeminfo
      let buildInfo = ["Autobuilder-Version: " ++ maybe "unknown" id autobuilderVersion] ++
                      ["Time: " ++ myTimeDiffToString elapsed] ++
                      maybe [] ((: []) . ("Memory: " ++)) (lookup "MemTotal" memInfo) ++
                      maybe [] ((: []) . ("CPU: " ++)) (lookup "model name" cpuInfo) ++
                      ["CPU count: " ++ (show . length . lookupAll "processor" $ cpuInfo)] ++
                      maybe [] ((: []) . ("CPU MHz: " ++)) (lookup "cpu MHz" cpuInfo) ++
                      maybe [] ((: []) . ("CPU cache: " ++)) (lookup "cache size" cpuInfo)
      let buildInfo' = buildInfo ++ maybe [] (\ name -> ["Host: " ++ name]) hostname
      let fields' = sinkFields (== "Files") (Paragraph $ fields ++ [Field ("Build-Info", "\n " ++ consperse "\n " buildInfo')])
      replaceFile (Debian.Repo.path changes) (show (Control [fields']))
      return changes

-- |Move this to {-Debian.-} Control
sinkFields :: (Eq a) => (a -> Bool) -> Paragraph' a -> Paragraph' a
sinkFields f (Paragraph fields) =
    let (a, b) = partition f' fields in Paragraph (b ++ a)
    where f' (Field (name, _)) = f name
          f' (Comment _) = False

-- |Download the package's build dependencies into /var/cache
downloadDependencies :: OSImage -> DebianBuildTree -> [String] -> [PkgVersion] -> TIO (Either String [Output])
downloadDependencies os source extra versions =
    do vers <- liftIO (evaluate versions)
       vEPutStrBl 1 . ("versions: " ++) . show $! vers
       (out, codes) <- liftIO (lazyCommand command L.empty) >>=
                       vMessage 0 ("Downloading build dependencies into " ++ rootPath (rootDir os)) >>=
                       dotOutput 100 >>= return . partitionResult
       case codes of
         [ExitSuccess] -> return (Right out)
         codes -> vMessage 0 ("FAILURE: " ++ command ++ " -> " ++ show codes ++ "\n" ++ outputToString out) () >>
                  return (Left ("FAILURE: " ++ command ++ " -> " ++ show codes))
    where
      command = ("chroot " ++ rootPath root ++ " bash -c " ++
                 "\"export DEBIAN_FRONTEND=noninteractive; unset LANG; " ++
                 (if True then aptGetCommand else pbuilderCommand) ++ "\"")
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes install --download-only " ++ consperse " " (map showPkgVersion versions ++ extra)
      path = envPath (topdir source)
      root = rootDir os

-- |Install the package's build dependencies.
installDependencies :: OSImage -> DebianBuildTree -> [String] -> [PkgVersion] -> TIO (Either String [Output])
installDependencies os source extra versions =
    do (out, codes) <- liftIO (lazyCommand command L.empty) >>=
                       vMessage 0 ("Installing build dependencies into " ++ rootPath (rootDir os)) >>=
                       dotOutput 100 >>= return . partitionResult
       case codes of
         [ExitSuccess] -> return (Right out)
         codes -> vMessage 0 ("FAILURE: " ++ command ++ " -> " ++ show codes ++ "\n" ++ outputToString out) () >>
                  return (Left ("FAILURE: " ++ command ++ " -> " ++ show codes))
    where
      command = ("chroot " ++ rootPath root ++ " bash -c " ++
                 "\"export DEBIAN_FRONTEND=noninteractive; unset LANG; " ++
                 (if True then aptGetCommand else pbuilderCommand) ++ "\"")
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes install " ++ consperse " " (map showPkgVersion versions ++ extra)
      path = envPath (topdir source)
      root = rootDir os

-- These two belongs in System.Unix.Process
partitionResult :: [Output] -> ([Output], [ExitCode])
partitionResult output =
    foldr f ([], []) output
    where
      f (Result r) (out, codes) = (out, (r : codes))
      f x (out, codes) = (x : out, codes)

outputToString [] = ""
outputToString (Stdout s : out) = B.unpack s ++ outputToString out
outputToString (Stderr s : out) = B.unpack s ++ outputToString out
outputToString (Result r : out) = show r ++ outputToString out

-- |Set a "Revision" line in the .dsc file, and update the .changes
-- file to reflect the .dsc file's new md5sum.  By using our newdist
-- program to update the pool, this line from the .dsc file is then
-- included in the package's entry in the Sources.gz file.  Then we
-- can compare the revision from the uploaded package with the current
-- TLA revision to decide whether to build.
setRevisionInfo :: DebianVersion -> Maybe String -> [PkgVersion] -> ChangesFile -> IO ChangesFile
setRevisionInfo sourceVersion revision versions changes {- @(Changes dir name version arch fields files) -} =
    case partition isDscFile (changeFiles changes) of
      ([file], otherFiles) ->
          do
            let dscFilePath = changeDir changes ++ "/" ++ changedFileName file
            newDscFile <- parseControlFromFile dscFilePath >>= return . either (error . show) addField
            replaceFile dscFilePath (show newDscFile)
            checksum <- md5sum dscFilePath
            case checksum of
              Left e -> error (show e)
              Right s ->
                  do
                    size <- getFileStatus dscFilePath >>= return . fileSize
                    let changes' = changes {changeFiles = (otherFiles ++ [file {changedFileMD5sum = s, changedFileSize = size}])}
                    Debian.Repo.save changes'
                    return changes'
      -- A binary only build will have no .dsc file
      ([], _) -> return changes
      (several, _) -> error ("Multiple .dsc files found in source package: " ++ show several)
    where
      addField (Control (Paragraph sourceInfo : binaryInfo)) =
          Control (newSourceInfo : binaryInfo)
          where newSourceInfo = raiseFields (/= "Files") (Paragraph (sourceInfo ++ [newField]))
      addField (Control []) = error "Invalid control file"
      newField = Field ("Revision", " " ++ newFieldValue)
      newFieldValue = maybe invalidRevision id revision ++ " " ++ show sourceVersion ++ " " ++ formatVersions versions
      formatVersions versions = consperse " " (map showPkgVersion versions)
      isDscFile file = isSuffixOf ".dsc" $ changedFileName file

-- |Decide whether to build a package.  We will build if the revision
-- is different from the revision of the uploaded source, or if any of
-- the build dependencies are newer than the versions which were
-- encoded into the uploaded version's control file.
buildDecision :: Tgt
              -> String
              -> Bool
              -> [String]
              -> ChangeLogEntry		-- The newest log entry from the source tree
              -> Maybe DebianVersion	-- builtVersion: the version already present in the repository
              -> Maybe DebianVersion	-- builtSrcVersion: the version of the source code that builtVersion was built from
              -> Maybe String		-- builtRevision: that version's revision string
              -> [PkgVersion]		-- builtDependencies: the list of of dependencies for that version
              -> SourcePackageStatus	-- releaseStatus: the status of the version in the repository
              -> DebianVersion		-- sourceVersion: the version number in the newest changelog entry of the source code
              -> Maybe String		-- sourceRevision: the revision string generated for the source package.  This will only be
					--   Nothing if the target is a Dir target, which is not suitable for uploading.
              -> [PkgVersion]		-- sourceDependencies: the list of build dependency versions computed from the build environment
              -> BuildDecision
buildDecision (Tgt _target) vendorTag forceBuild relaxDepends sourceLog
                  oldVersion oldSrcVersion _oldRevision builtDependencies releaseStatus
                  sourceVersion _sourceRevision sourceDependencies =
    case (forceBuild, oldVersion == Nothing) of
      (True, _) -> Yes "--force-build option is set"
      (False, True) -> Yes ("Initial build of version " ++ show sourceVersion)
      (False, False) ->
          case isJust oldSrcVersion of
            True ->
                case compare sourceVersion (fromJust oldSrcVersion) of
                  GT -> Yes ("Source version (" ++ show sourceVersion ++ ") is newer than released source version (" ++ show (fromJust oldSrcVersion) ++ ")")
                  LT -> No ("Source version (" ++ show sourceVersion ++ ") is trumped by released source version (" ++ show (fromJust oldSrcVersion) ++ ")")
                  EQ -> sameSourceTests
            False ->
                case compare (dropTag vendorTag sourceVersion) (dropTag vendorTag (fromJust oldVersion)) of
                  GT -> Yes ("Source version (" ++ show sourceVersion ++ ") is newer than released source version (" ++ show (fromJust oldVersion) ++ ")")
                  LT -> No ("Source version (" ++ show sourceVersion ++ ") is trumped by released source version (" ++ show (fromJust oldVersion) ++ ")")
                  EQ ->
                      case dropTag vendorTag sourceVersion == sourceVersion of
                        False -> Yes ("Source version (" ++ show sourceVersion ++ ") is tagged, and old source version was not recorded")
                        True -> sameSourceTests
    where
      -- Build decision tests for when the version number of the
      -- source hasn't changed.  Note that the source itself may have
      -- changed, but we don't ask the SCCS whether that has happened.
      -- This is a design decision which avoids building from source
      -- that might have been checked in but isn't ready to be
      -- uploaded to the repository.  Note that if the build
      -- dependencies change the package will be built anyway, so we
      -- are not completely protected from this possibility.
      sameSourceTests =
          case () of
            _ | badDependencies /= [] ->
                  Error ("Missing build dependencies: " ++ 
                         concat (intersperse ", " (map (\ ver -> getName ver ++ " >= " ++ show (getVersion ver)) badDependencies)) ++
                         " (use --relax-depends to ignore.)")
              | autobuiltDependencies /= [] && isNothing oldSrcVersion ->
		  -- If oldSrcVersion is Nothing, the autobuilder didn't make the previous build
                  -- so there are no recorded build dependencies.  In that case we don't really
                  -- know whether a build is required, so we could go either way.  The decision
                  -- here is to only built if some of the build dependencies were built by the
                  -- autobuilder (so their version numbers have been tagged by it.)
                  Auto ("Build dependencies changed:\n" ++ buildDependencyChangeText autobuiltDependencies)
              | (revvedDependencies ++ newDependencies) /= [] && isJust oldSrcVersion ->
                  -- If the package *was* previously built by the autobuilder we rebuild when any
                  -- of its build dependencies are revved or new ones appear.
                  Auto ("Build dependencies changed:\n" ++ buildDependencyChangeText (revvedDependencies ++ newDependencies))
              | releaseStatus == Indep ->
                  -- The binary packages are missing, we need an arch only build.
                  Arch ("Version " ++ maybe "Nothing" show oldVersion ++ " needs arch only build.")
              | releaseStatus == All ->
                  No ("Version " ++ show sourceVersion ++ " is already in release.")
              | True -> 
                  error ("Unexpected releaseStatus: " ++ show releaseStatus)
      buildDependencyChangeText dependencies =
          "  " ++ consperse "\n  " lines
          where
            lines = map (\ (built, new) -> show built ++ " -> " ++ show new) (zip builtVersions dependencies)
            builtVersions = map (findDepByName builtDependencies) dependencies
            findDepByName builtDependencies new = find (\ old -> getName new == getName old) builtDependencies
      -- The list of the revved and new dependencies which were built by the autobuilder.
      autobuiltDependencies = filter isTagged (revvedDependencies ++ newDependencies)
      -- If we are deciding whether to rebuild the same version of the source package,
      -- this function checks the status of the build dependencies.  If any are older
      -- now than when the package was built previously, it is a fatal error.  Probably
      -- the sources.list changed so that build dependency versions are no longer
      -- available, or some of the build dependencies were never built for the current
      -- build architecture.  If any are younger, we need to rebuild the package.
      -- buildDependencyStatus :: ([PkgVersion], [PkgVersion], [PkgVersion], [PkgVersion])
      (badDependencies, revvedDependencies, newDependencies, _unchangedDependencies) =
          (bad, changed, new, unchanged)
          where
            -- If a dependency is older than the one we last built with it is an error.
            (bad, notBad) = partition isOlder sourceDependencies'
            isOlder new = maybe False (\ built -> getVersion built > getVersion new) (builtVersion new)
            -- If a dependency is newer it generally triggers a rebuild.
            (changed, notChanged) = partition isNewer notBad
            isNewer new = maybe False (\ built -> getVersion built < getVersion new) (builtVersion new)
	    -- Dependencies which we have never seen before also generally trigger a rebuild.
            (new, unchanged) = partition (isNothing . builtVersion) notChanged
	    -- What version of this dependency was most recently used to build?
            builtVersion x = maybe Nothing (\ ver -> Just (PkgVersion (getName x) ver)) (Map.findWithDefault Nothing (getName x) builtDeps)
	    builtDeps = Map.fromList (map (\ p -> (getName p, Just (getVersion p))) builtDependencies)
      -- Remove the current package and any package in the relaxDepends list
      -- from the list of build dependencies which can trigger a rebuild.
      sourceDependencies' = filterDepends (logPackage sourceLog : relaxDepends) sourceDependencies
      filterDepends :: [String] -> [PkgVersion] -> [PkgVersion]
      filterDepends relaxDepends sourceDependencies =
          filter (\ ver -> not (elem (getName ver) relaxDepends)) sourceDependencies
      isTagged :: PkgVersion -> Bool
      isTagged dep = isJust . snd . parseTag vendorTag . getVersion $ dep
