{-# LANGUAGE PatternSignatures #-}
-- |A Target represents a particular set of source code and the
-- methods to retrieve and update it.
--
-- Author: David Fox <ddssff@gmail.com>
module Target
    ( Target(..)
    , targetName	-- Target -> String
    , changelogText	-- Tgt -> Maybe String -> [PkgVersion] -> String
    , prepareTarget	-- ReleaseName -> OSImage -> Int -> Int -> Tgt -> IO Target
    , readSpec		-- Bool -> FilePath -> Bool -> [DistroCache] -> String -> IO Tgt
    , buildTargets
    , showTargets 
    , targetDocumentation
    ) where

import		 Control.Exception
import		 Control.Monad.RWS hiding (All)
import		 Debian.Cache
import		 Debian.Control
import qualified Debian.Control.ByteString as B
import qualified Debian.Control.String as S(fieldValue)
import		 Debian.Dependencies
import qualified Debian.GenBuildDeps as GenBuildDeps
import		 Debian.TIO
import		 Debian.IO
import		 Debian.Shell
import		 Debian.Local.Changes
import		 Debian.Local.Insert
import		 Debian.OSImage
import		 Debian.Package
import		 Debian.Repo
import		 Debian.Time
import		 Debian.Types
import		 Debian.Types.SourceTree
import		 Debian.SourceTree
import		 Debian.VersionPolicy
--import		 Extra.Bool
import		 Extra.Either
import		 Extra.List
import		 Extra.Misc
import		 BuildTarget
import		 BuildTarget.Apt
import		 BuildTarget.Darcs
import		 BuildTarget.DebDir
import		 BuildTarget.Hg
import		 BuildTarget.Proc
import		 BuildTarget.Quilt
import		 BuildTarget.SourceDeb
import		 BuildTarget.Svn
import		 BuildTarget.Tla
import		 BuildTarget.Uri
--import		 Control.Monad
import		 Control.Monad.Reader
import		 Linspire.Unix.Process hiding (processOutput)
import qualified Data.ByteString.Char8 as B
import		 Data.List
import		 Data.Maybe
import qualified Data.Set as Set
import		 Debian.Relation.ByteString as B
import		 Debian.Version
import		 Params
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
            , BuildTarget.Apt.documentation
            , BuildTarget.Darcs.documentation
            , BuildTarget.DebDir.documentation
            , BuildTarget.Hg.documentation
            , BuildTarget.Proc.documentation
            , BuildTarget.Quilt.documentation
            , BuildTarget.SourceDeb.documentation
            , BuildTarget.Svn.documentation
            , BuildTarget.Tla.documentation
            , BuildTarget.Uri.documentation ])

-- | Build target info.
data Target
    = Target { realSource :: Tgt		-- ^ The source code obtained from the SCCS
             , cleanSource :: DebianBuildTree	-- ^ The source code stripped of SCCS info
             , targetEntry :: ChangeLogEntry	-- ^ The contents of the most recent changelog entry
             , targetControl :: Control		-- ^ The contents of debian\/control
             , targetVersion :: DebianVersion	-- ^ The version number in debian\/changelog
             , targetRevision :: Maybe String	-- ^ The value computed by 'revision'
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
                  Left message -> vPutStrBl 0 message >> return Nothing
                  Right tree -> copySource tree >>= return . either (const Nothing) Just
{-
    do -- First we need to locate the original tree and see if it is a DebianSourceTree
       -- or a DebianBuildTree.
       debSource <- findDebianSourceTree (getTop target)
       vPutStrLn 2 stderr $ "debSource <- " ++ show debSource
       debBuild <- maybe (findDebianBuildTree (getTop target)) (const $ return Nothing) debSource
       vPutStrLn 2 stderr $ "debBuild <- " ++ show debBuild
       --debBuild <- findDebianBuildTree (getTop target)
       --debSource <- maybe (findDebianSourceTree (getTop target)) (return . Just . debTree) debBuild
       -- Now we need to copy the tree so it is a DebianBuildTree.
       maybe (maybe Nothing (Just . copySource) debSource) (Just . copyBuild) debBuild
-}
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
    ("  * " ++ logText spec revision ++
     "\n  * Build dependency changes:" ++
     prefix ++ consperse prefix padded ++ "\n")
    where
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
    tio (vPutStrBl 0 (text ++ ":")) >> mapRWST (local (appPrefix " ")) readSpec'
    where
      readSpec' =
          case text of
            'a':'p':'t':':' : target -> prepareApt top flush ifSourcesChanged distros target
            'd':'a':'r':'c':'s':':' : target -> tio $ prepareDarcs debug top flush target
            'd':'e':'b':'-':'d':'i':'r':':' : target ->
                do pair <- parsePair debug target
                   case pair of
                     Left message -> return (Left message)
                     Right (upstream, debian) -> tio $ prepareDebDir debug top flush upstream debian
            'd':'i':'r':':' : target -> tio $ prepareDir debug top flush (rootEnvPath target)
            'h':'g':':' : target -> tio $ prepareHg debug top flush target
            'q':'u':'i':'l':'t':':' : target ->
                do pair <- parsePair debug target
                   case pair of
                     Left message -> return (Left message)
                     Right (base, patch) -> tio $ prepareQuilt top flush base patch
            's':'o':'u':'r':'c':'e':'d':'e':'b':':' : target ->
                readSpec debug top flush ifSourcesChanged distros target >>= tio . either (return . Left) prepareSourceDeb
            's':'v':'n':':' : target -> tio $ prepareSvn debug top flush target
            't':'l':'a':':' : target -> tio $ prepareTla top flush target
            'u':'r':'i':':' : target -> tio $ prepareUri debug top flush target
            'p':'r':'o':'c':':' : target ->
                readSpec debug top flush ifSourcesChanged distros target >>=
                tio . either (return . Left) (prepareProc top flush)
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
      tio (vPutStrBl 0 "Building all targets:")
      failed <- {- setStyle (addPrefix " ") $ -} buildLoop cleanOS globalBuildDeps (length targetList) (targetList, [])
      return (localRepo, failed)
      --buildAll cleanOS targetList globalBuildDeps
    where
      -- Retrieve and/or update the source code of all the targets before building.
      --prepareAllTargetSource :: OSImage -> TIO [Target]
      prepareAllTargetSource cleanOS =
          do
            vPutStrBl 0 "Assembling clean source tree for each target:"
            ({- debugStyle . -} iStyle) $ countAndPrepareTargets cleanOS targetSpecs
          where
            iStyle = setStyle (appPrefix " ")
      -- Execute the target build loop until everything is built
      --buildLoop :: OSImage -> Relations -> Int -> ([Target], [Target]) -> AptIO [Target]
      buildLoop _ _ _ ([], failed) = return failed
      buildLoop cleanOS globalBuildDeps count (unbuilt, failed) =
          do
            targetGroups <- tio $ chooseNextTarget globalBuildDeps unbuilt
            tio (vPutStrBl 1 ("\n\n" ++ makeTable targetGroups))
            case targetGroups of
              (group@(target : blocked) : other) ->
                  tio (vEPutStrBl 0 (printf "[%2d of %2d] TARGET: %s\n"
                                     (count - length (concat targetGroups) + 1) count (show target))) >>
                  mapRWST (local (appPrefix " ")) (buildTarget' target) >>=
                  either (\ e -> do tio $ vEPutStrBl 0 ("Package build failed: " ++ e)
                                    tio $ vEPutStrBl 0 ("Discarding " ++ show target ++ " and its dependencies:\n  " ++
                                                        concat (intersperse "\n  " (map show blocked)))
                                    buildLoop cleanOS globalBuildDeps count (concat other, group ++ failed))
                         (\ _ -> do cleanOS' <- updateEnv cleanOS
                                    case cleanOS' of
                                      Left e -> error ("Failed to update clean OS: " ++ e)
                                      Right cleanOS'' ->
                                          buildLoop cleanOS'' globalBuildDeps count (blocked ++ concat other, failed))
              _ -> return failed
          where
            makeTable groups =
                unlines . map (consperse " ") . columns $ ["Ready", "Blocked"] : ["-----", "-------"] : map makeRow groups
            makeRow :: [Target] -> [String]
            makeRow group = [(targetName . head $ group), (consperse " " . map targetName . tail $ group)]
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
chooseNextTarget :: Relations -> [Target] -> TIO [[Target]]
chooseNextTarget globalBuildDeps targets =
    getDependencyInfo targets >>= return . buildGroups
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
               bad -> (vPutStrBl 0
                       ("Unable to retrieve build dependency info for some targets:\n  " ++
                        concat (intersperse "\n  " (map (\ (target, error) -> show target ++ ": " ++ either id (const "") error) bad))))
             return depInfo

      -- Return the set of build dependency groups, each of which
      -- starts with a target that is ready to build followed by
      -- targets which are blocked by the first target.
      buildGroups :: [(Target, GenBuildDeps.DepInfo)] -> [[Target]]
      buildGroups pairs =
          let buildable = GenBuildDeps.buildable depends pairs in
          map (map fst) buildable
      getDepInfo :: Relations -> DebianBuildTree -> TIO (Either String GenBuildDeps.DepInfo)
      getDepInfo globalBuildDeps buildTree =
          do
            --let sourceTree = debTree buildTree
            let controlPath = appendPath "/debian/control" (debdir buildTree)
            info <- lift $ GenBuildDeps.genDep (outsidePath controlPath)
            -- My.ePutStr ("getDepInfo " ++ show target ++ ": " ++ show info)
            return $ either Left (Right . addRelations globalBuildDeps) info
      addRelations :: Relations -> GenBuildDeps.DepInfo -> GenBuildDeps.DepInfo
      addRelations moreRels (name, relations, bins) = (name, relations ++ moreRels, bins)
      depends (_, depInfo1) (_, depInfo2) = GenBuildDeps.compareSource depInfo1 depInfo2

{-
showTargets targets =
    hPutStrLn stderr (show (length targets) ++ " Targets:\n") >>
    mapM_ (hPutStrLn stderr) (map ("  " ++) targets) >> hPutStrLn stderr ""
-}

--showTargets :: Show a => [a] -> IO ()
showTargets targets =
    vPutStr 0 ("\n\n" ++ 
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

instance Show BuildDecision where
    show (Yes reason) = "Yes - " ++ reason
    show (No reason) = "No - " ++ reason
    show (Arch reason) = "Yes - " ++ reason
    show (Auto reason) = "Yes - " ++ reason

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
      tio (vPutStrBl 0 "Loading package lists and searching for build dependency solution...")
      solutions <- tio (iStyle $ buildDepSolutions' (Params.preferred params) cleanOS globalBuildDeps debianControl)
      case solutions of
        Left excuse -> do tio (vPutStrBl 0 ("Couldn't satisfy build dependencies\n " ++ excuse))
                          return $ Left ("Couldn't satisfy build dependencies\n" ++ excuse)
        Right [] -> error "Internal error"
        Right ((count, sourceDependencies) : _) ->
            do tio (vPutStr 0 ("Build dependency solution #" ++ show count))
               tio (vPutStr 2 (concat (map (("\n  " ++) . show) sourceDependencies)))
               tio (vPutStrBl 0 "\n")
               -- Get the newest available version of a source package,
               -- along with its status, either Indep or All
               let (releaseControlInfo, releaseStatus, message) = getReleaseControlInfo cleanOS packageName
               tio (vPutStrBl 2 message)
               tio (vEPutStrBl 0 ("Status of " ++ packageName ++ maybe "" (\ p -> "-" ++ show (packageVersion . sourcePackageID $ p)) releaseControlInfo ++
                             ": " ++ explainSourcePackageStatus releaseStatus))
               --My.ePutStr ("Target control info:\n" ++ show releaseControlInfo)
               -- Get the revision info of the package currently in the dist
               let oldVersion = maybe Nothing (Just . getOldVersion) releaseControlInfo
               let (oldRevision, oldDependencies) = maybe (Nothing, []) getOldRevision releaseControlInfo
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
               let sourcePackages = aptSourcePackagesSorted poolOS [packageName]
               let newVersion = computeNewVersion params sourcePackages releaseControlInfo sourceVersion
{-
               tio (vPutStrBl 0 ("Version number for new build: " ++ show sourceVersion ++ " -> " ++ show newVersion) >>
                    vPutStrBl 0 ("Params.isDevelopmentRelease: " ++ show (Params.isDevelopmentRelease params)) >>
                    vPutStrBl 0 ("sliceName (Params.baseRelease params): " ++ show (sliceName (Params.baseRelease params))) >>
                    vPutStrBl 0 ("Params.extraReleaseTag params: " ++ show (Params.extraReleaseTag params)) >>
                    vPutStrBl 0 ("Available versions: " ++ show (map (maybe Nothing (Just . parseDebianVersion . B.unpack) . (fieldValue "Version" . sourceParagraph)) sourcePackages)))
-}
               let ignoredBuildDeps = filterPairs (logPackage sourceLog) (Params.relaxDepends params)
               let decision =
                       buildDecision (realSource target) (Params.vendorTag params)
                                         (Params.forceBuild params) ignoredBuildDeps sourceLog
                                         oldVersion oldRevision oldDependencies releaseStatus
                                         sourceVersion sourceRevision sourceDependencies
               tio (vPutStrBl 0 ("Build decision: " ++ show decision))
               -- FIXME: incorporate the release status into the build decision
               case newVersion of
                 Left message ->
                    return (Left message)
                 Right version ->
                    case decision of
                      No _ -> return (Right repo)
                      Yes _ ->  buildPackage params cleanOS (Just version) oldDependencies sourceRevision sourceDependencies target None repo sourceLog
                      Arch _ -> buildPackage params cleanOS oldVersion oldDependencies sourceRevision sourceDependencies target releaseStatus repo sourceLog
                      Auto _ -> buildPackage params cleanOS (Just version) oldDependencies sourceRevision sourceDependencies target None repo sourceLog
    where
      --buildTree = maybe (error $ "Invalid target for build: " ++ show target) id (getBuildTree . cleanSource $ target)
      packageName = Target.targetName target
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

-- | Build a package and upload it to the local repository.
buildPackage :: Params.Params -> OSImage -> Maybe DebianVersion -> [PkgVersion] -> Maybe String -> [PkgVersion] -> Target -> SourcePackageStatus -> LocalRepository -> ChangeLogEntry -> AptIO (Either String LocalRepository)
buildPackage params cleanOS newVersion oldDependencies sourceRevision sourceDependencies target status repo sourceLog =
    checkDryRun >> (tio prepareImage) >>= (tio . logEntry) >>= (tio . build) >>= (tio . find) >>= upload
    where
      checkDryRun = when (Params.dryRun params)  (do tio (vPutStrBl 0 "Not proceeding due to -n option.")
                                                     io (exitWith ExitSuccess))
      prepareImage = prepareBuildImage params cleanOS sourceDependencies buildOS target (Params.strictness params)
      logEntry :: Either String DebianBuildTree -> TIO (Either String DebianBuildTree)
      logEntry (Left message) = return (Left message)
      logEntry (Right buildTree) = 
          case Params.noClean params of
            False -> lift $ maybeAddLogEntry buildTree newVersion >> return (Right buildTree)
            True -> return (Right buildTree)
      build :: Either String DebianBuildTree -> TIO (Either String (DebianBuildTree, TimeDiff))
      build (Left message) = return (Left message) 
      build (Right buildTree) =
          case realSource target of
            Tgt t -> do result <- buildPkg (Params.noClean params) (Params.setEnv params) buildOS buildTree status t
                        case result of
                          Left message -> return (Left message)
                          Right elapsed -> return (Right (buildTree, elapsed))
      find (Left message) = return (Left message)
      find (Right (buildTree, elapsed)) =
          lift $ Debian.SourceTree.findChanges buildTree >>= return . either Left (\ changesFile -> Right (changesFile, elapsed))
      upload :: Either String (ChangesFile, TimeDiff) -> AptIO (Either String LocalRepository)
      upload (Left message) = return . Left $ "Upload failed: " ++ message
      upload (Right (changesFile, elapsed)) = doLocalUpload elapsed changesFile
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
          let (Paragraph fields) = Debian.Local.Changes.changeInfo changes in
          let info' = map (setDist name) fields in
          changes { Debian.Local.Changes.changeInfo = Paragraph info'
                  , Debian.Local.Changes.changeRelease = name }
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
                io . setRevisionInfo newVersion sourceRevision sourceDependencies
            -- Upload to the local apt repository
            tio (uploadLocal repo changesFile')
            -- The upload to the local repository is done even when
            -- the --dry-run flag is given.  Or it would be if we
            -- didn't exit when the first buildworthy target is found.
            (_, errors) <- scanIncoming True Nothing repo
            case errors of
              [] -> return . Right $ repo
              _ -> return . Left $ "Local upload failed:\n" ++ showErrors (map snd errors)
      buildOS = Debian.OSImage.chrootEnv cleanOS (Params.dirtyRoot params)

-- |Prepare the build image by copying the clean image, installing
-- dependencies, and copying the clean source tree.  For a lax build
-- these operations take place in a different order from other types
-- of builds.  For lax: dependencies, then image copy, then source
-- copy.  For other: image copy, then source copy, then dependencies.
prepareBuildImage :: Params.Params -> OSImage -> [PkgVersion] -> OSImage -> Target -> Params.Strictness -> TIO (Either String DebianBuildTree)
prepareBuildImage params cleanOS sourceDependencies buildOS target@(Target (Tgt tgt) _ _ _ _ _) Params.Lax =
    do result <- installDependencies cleanOS (cleanSource target) buildDepends sourceDependencies
       case result of
         Left message -> return (Left message)
         Right _ ->
             case noClean of
               True -> do tree <- findOneDebianBuildTree newPath
                          case tree of
                            Nothing -> return . Left $ "No build tree at " ++ show newPath
                            Just tree -> return . Right $ tree
               False -> vBOL 0 >>
                        vPutStr 0 "Syncing buildOS" >>
                        Debian.OSImage.syncEnv cleanOS buildOS >>=
                        (const (prepareCopy tgt (cleanSource target) newPath))
    where
      buildDepends = (Params.buildDepends params)
      noClean = Params.noClean params
      newPath = EnvPath {envRoot = rootDir buildOS, envPath = envPath oldPath}
      oldPath = topdir . cleanSource $ target
prepareBuildImage params cleanOS sourceDependencies buildOS target@(Target (Tgt tgt) _ _ _ _ _) _ =
    do
      buildTree <- 
          case noClean of
            False -> prepareCopy tgt (cleanSource target) newPath
            True -> findOneDebianBuildTree newPath >>= return . maybe (Left "build tree not found") Right
      case buildTree of
        Left message -> return (Left message)
        Right buildTree -> iStyle $ downloadDependencies cleanOS buildTree buildDepends sourceDependencies
      case noClean of
        False -> vBOL 0 >> vPutStr 0 "Syncing buildOS" >>Debian.OSImage.syncEnv cleanOS buildOS
        True -> return buildOS
      case buildTree of
        Left message -> return (Left message)
        Right buildTree -> iStyle $ installDependencies buildOS buildTree buildDepends sourceDependencies
      return buildTree
    where
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
      binaryPackages = Debian.OSImage.binaryPackages cleanOS (nub . concat . map sourcePackageBinaryNames $ sourcePackages)
      sourcePackages = sortBy compareVersion . Debian.OSImage.sourcePackages cleanOS $ [packageName]
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
                 || unableToCheckUDebs
                        || Set.difference required (Set.union availableDebs availableUDebs) == Set.empty
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

getOldRevision :: SourcePackage -> (Maybe String, [PkgVersion])
getOldRevision package = 
    maybe (Nothing, []) (parseRevision . B.unpack) (S.fieldValue "Revision" . sourceParagraph $ package)
    where
      parseRevision s =
          case words s of
            [] -> (Nothing, [])
            (revision : buildDeps) -> (Just revision, map readPkgVersion buildDeps)

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

buildDepSolutions' :: [String] -> OSImage -> Relations -> Control -> TIO (Either String [(Int, [PkgVersion])])
buildDepSolutions' preferred os globalBuildDeps debianControl =
    do
      arch <- lift $ buildArchOfEnv (rootDir os)
      case GenBuildDeps.buildDependencies debianControl of
        Left message -> return (Left message)
        Right (_, relations, _) ->
            do let relations' = filterRelations arch (relations ++ globalBuildDeps)
               let relations'' = computeBuildDeps os arch relations'
               vPutStrBl 2 $ ("Build dependency relations:\n " ++
                              concat (intersperse "\n " (map (\ (a, b) -> show a ++ " -> " ++ show b) (zip relations' relations''))))
               -- Do any of the dependencies require packages that simply don't
               -- exist?  If so we don't have to search for solutions, there
               -- aren't any.
               if any (== []) relations'' then
                   return $ Left ("Unsatisfiable dependencies: " ++
                                  show (catMaybes (map unsatisfiable (zip relations' relations'')))) else
                   -- Do not stare directly into the solutions!  Your head will
                   -- explode (because there may be a lot of them.)
                   return $ Debian.Dependencies.solutions (available relations'') relations'' preferred arch
    where
      unsatisfiable (original, []) = Just original
      unsatisfiable _ = Nothing

      available relations = uniquify (Debian.OSImage.binaryPackages os (packagesOfRelations relations))
      -- If the same package/version appears more than once, we can
      -- ignore all but one since there is an assumption in Debian
      -- that packages with the same name and version are the same.
      uniquify versions =
          map head . groupBy eq . sortBy cmp $ versions
          where cmp a b = compare (packageName (packageID b), packageVersion (packageID b))
				  (packageName (packageID a), packageVersion (packageID a))
		eq a b = packageName (packageID a) == packageName (packageID b)
      --printSoln rels = mapM_ printRel rels
      --printRel x = vPutStrLn 0 (show x)

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
      removeFile (Debian.Local.Changes.path changes)
      writeFile (Debian.Local.Changes.path changes) (show (Control [fields']))
      return changes

-- |Move this to {-Debian.-} Control
sinkFields :: (Eq a) => (a -> Bool) -> Paragraph' a -> Paragraph' a
sinkFields f (Paragraph fields) =
    let (a, b) = partition f' fields in Paragraph (b ++ a)
    where f' (Field (name, _)) = f name
          f' (Comment _) = False

-- |Extract the packages named in a dependency relation
packagesOfRelations :: Relations -> [PkgName]
packagesOfRelations relations =
    map packageOfRelation (concat relations)
    where packageOfRelation (Rel name _ _) = name

-- |Download the package's build dependencies into /var/cache
downloadDependencies :: OSImage -> DebianBuildTree -> [String] -> [PkgVersion] -> TIO (Either String [Output])
downloadDependencies os source extra versions =
    do vers <- liftIO (evaluate versions)
       vPutStrBl 1 . ("versions: " ++) . show $! vers
       runTaskAndTest (builddepStyle (commandTask command))
    where
      command = ("chroot " ++ rootPath root ++ " bash -c " ++
                 "\"export DEBIAN_FRONTEND=noninteractive; unset LANG; " ++
                 (if True then aptGetCommand else pbuilderCommand) ++ "\"")
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes install --download-only " ++ consperse " " (map showPkgVersion versions ++ extra)
      builddepStyle = (setStart (Just ("Downloading build dependencies into " ++ rootPath (rootDir os))) .
                       setError (Just (\ _ -> "Could not satisfy build dependencies.")))
      path = envPath (topdir source)
      root = rootDir os

-- |Install the package's build dependencies.
installDependencies :: OSImage -> DebianBuildTree -> [String] -> [PkgVersion] -> TIO (Either String [Output])
installDependencies os source extra versions =
    runTaskAndTest (builddepStyle (commandTask command))
    where
      command = ("chroot " ++ rootPath root ++ " bash -c " ++
                 "\"export DEBIAN_FRONTEND=noninteractive; unset LANG; " ++
                 (if True then aptGetCommand else pbuilderCommand) ++ "\"")
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes install " ++ consperse " " (map showPkgVersion versions ++ extra)
      builddepStyle = (setStart (Just ("Installing build dependencies into " ++ rootPath (rootDir os))) .
                       setError (Just (\ _ -> "Could not satisfy build dependencies.")))
      path = envPath (topdir source)
      root = rootDir os

-- |Set a "Revision" line in the .dsc file, and update the .changes
-- file to reflect the .dsc file's new md5sum.  By using our newdist
-- program to update the pool, this line from the .dsc file is then
-- included in the package's entry in the Sources.gz file.  Then we
-- can compare the revision from the uploaded package with the current
-- TLA revision to decide whether to build.
setRevisionInfo :: Maybe DebianVersion -> Maybe String -> [PkgVersion] -> ChangesFile -> IO ChangesFile
setRevisionInfo _ revision versions changes {- @(Changes dir name version arch fields files) -} =
    case partition isDscFile (changeFiles changes) of
      ([file], otherFiles) ->
          do
            let dscFilePath = changeDir changes ++ "/" ++ changedFileName file
            newDscFile <- parseControlFromFile dscFilePath >>= return . either (error . show) addField
            removeFile dscFilePath
            writeFile dscFilePath (show newDscFile)
            checksum <- md5sum dscFilePath
            case checksum of
              Left e -> error (show e)
              Right s ->
                  do
                    size <- getFileStatus dscFilePath >>= return . fileSize
                    let changes' = changes {changeFiles = (otherFiles ++ [file {changedFileMD5sum = s, changedFileSize = size}])}
                    Debian.Local.Changes.save changes'
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
      newFieldValue = maybe invalidRevision id revision ++ " " ++ formatVersions versions
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
              -> Maybe DebianVersion	-- oldVersion: the version already present in the repository
              -> Maybe String		-- oldRevision: that version's revision string
              -> [PkgVersion]		-- oldDependencies: the list of of dependencies for that version
              -> SourcePackageStatus	-- releaseStatus: the status of the version in the repository
              -> DebianVersion		-- sourceVersion: the version number in the newest changelog entry of the source code
              -> Maybe String		-- sourceRevision: the revision string generated for the source package.  This will only be
					--   Nothing if the target is a Dir target, which is not suitable for uploading.
              -> [PkgVersion]		-- sourceDependencies: the list of build dependency versions computed from the build environment
              -> BuildDecision
buildDecision (Tgt _target) vendorTag forceBuild relaxDepends sourceLog
                  oldVersion _oldRevision oldDependencies releaseStatus
                  sourceVersion _sourceRevision sourceDependencies =
    case () of
      _ | forceBuild ->
            Yes "--force-build option is set"
      _ | oldVersion == Nothing ->
            Yes ("Initial build of version " ++ show sourceVersion)
      _ | sourceVersion < fromJust (baseVersion oldVersion) ->
            No ("Source version (" ++ show sourceVersion ++ ") " ++
                "trumped by release version (" ++ (maybe "Nothing" show) (baseVersion oldVersion) ++ ")" ++
                "(dropTag " ++ show vendorTag ++ " " ++ show oldVersion ++ " = " ++ show (dropTag vendorTag (fromJust oldVersion)) ++ ")" ++
                "(dropTag " ++ show vendorTag ++ " " ++ show sourceVersion ++ " = " ++ show (dropTag vendorTag sourceVersion) ++ ")")
      _ | compareSourceAndDist vendorTag sourceVersion (fromJust oldVersion) == GT ->
            Yes ("Source version (" ++ show (parseTag vendorTag sourceVersion) ++ ") " ++
                 "is newer than release version (" ++ maybe "Nothing" (show . parseTag vendorTag) oldVersion ++ ")")
      _ | buildDependencyChanges /= Set.empty ->
            Auto ("Build dependencies changed:\n" ++ buildDependencyChangeText)
      _ | releaseStatus == Indep ->
            Arch ("Version " ++ maybe "Nothing" show oldVersion ++ " needs arch only build.")
      _ | releaseStatus == All ->
            No ("Version " ++ show sourceVersion ++ " is already in release.")
      _ -> 
            error ("Unexpected releaseStatus: " ++ show releaseStatus)
    where
      -- The autobuilder will add a suffix to a version number when it
      -- needs to do an automated rebuild.  This suffixed version
      -- number ends up as the release ("old") version number, and
      -- should generally be stripped off to get back the source
      -- version number for comparison.  However, it may be that the
      -- source version also includes such a suffix, if the developer
      -- chooses a version number of that format.  Consider:
      -- 
      --   sourceVersion = 1.1-0r1cnr1
      --   oldVersion = Just (1.1-0r1cnr2)  (due to an automated rebuild)
      -- 
      -- If we need to do an arch-only build, we need to do it for
      -- version 1.1-0r1cnr2.
      baseVersion Nothing = Nothing
      baseVersion (Just oldVersion) =
          if strippedOldVersion == strippedSourceVersion then Just sourceVersion else Just strippedOldVersion
          where 
            strippedOldVersion = dropTag vendorTag oldVersion
      strippedSourceVersion = dropTag vendorTag sourceVersion
      buildDependencyChangeText =
          "  " ++ consperse "\n  " lines
          where
            lines = map (\ (old, new) -> show old ++ " -> " ++ show new) (zip oldVersions newDependencies)
            oldVersions = map (findDepByName oldDependencies) newDependencies
            newDependencies = Set.toList $ buildDependencyChanges
            findDepByName oldDependencies new = find (\ old -> getName new == getName old) oldDependencies
      buildDependencyChanges =
          -- Remove the current package and any package in the relaxDepends list
          -- from the list of build dependencies which can trigger a rebuild.
          Set.difference (Set.fromList sourceDependencies') (Set.fromList oldDependencies)
          where sourceDependencies' = filterDepends (logPackage sourceLog : relaxDepends) sourceDependencies
      filterDepends :: [String] -> [PkgVersion] -> [PkgVersion]
      filterDepends relaxDepends sourceDependencies =
          filter (\ ver -> not (elem (getName ver) relaxDepends)) sourceDependencies
