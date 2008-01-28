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
import		 Control.Monad.Trans
import		 Debian.Cache
import		 Debian.Control
import qualified Debian.Control.ByteString as B
import qualified Debian.Control.String as S(fieldValue)
import		 Debian.Dependencies
import qualified Debian.GenBuildDeps as GenBuildDeps
import		 Debian.IO
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
import		 Extra.Bool
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
import		 Control.Monad
import qualified Data.ByteString.Char8 as B
import		 Data.List
import		 Data.Maybe
import qualified Data.Set as Set
import		 Debian.Relation.ByteString as B
import		 Debian.Version
import		 Params
import		 System.Directory
import		 System.Locale
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

countAndPrepareTargets :: OSImage -> [Tgt] -> AptIO [Target]
countAndPrepareTargets os targets =
    countTasks (zip (map show targets) (map (prepareTarget os) targets))

-- |Prepare a target for building in the given environment.  At this
-- point, the target needs to be a DebianSourceTree or a
-- DebianBuildTree. 
prepareTarget :: OSImage -> Tgt -> AptIO Target
prepareTarget os tgt@(Tgt source) =
    do tree <- prepareBuild os source >>= 
               return . maybe (error $ "Could not find Debian build tree for " ++ show source) id
       let ctl = control tree
           latest = entry tree
           ver = logVersion latest
       rev <- BuildTarget.revision source
       return $ Target tgt tree latest ctl ver rev

-- |'prepareBuild' returns a Debian build tree for a target with all
-- the revision control files associated with the old target removed.
-- This ensures that the tarball and\/or the .diff.gz file in the deb
-- don't contain extra junk.  It also makes sure that debian\/rules is
-- executable.
prepareBuild :: (BuildTarget t) => OSImage -> t -> AptIO (Maybe DebianBuildTree)
prepareBuild os target =
    do debBuild <- findOneDebianBuildTree (getTop target)
       case debBuild of
         Just tree -> copyBuild tree >>= return . Just
         Nothing ->
             do debSource <- findDebianSourceTree (getTop target)
                case debSource of
                  Nothing -> return Nothing
                  Just tree -> copySource tree >>= return . Just
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
      copySource :: DebianSourceTree -> AptIO DebianBuildTree
      copySource debSource =
          do let name = logPackage . entry $ debSource
                 dest = EnvPath (rootDir os) ("/work/build/" ++ name)
                 ver = Debian.Version.version . logVersion . entry $ debSource
                 newdir = escapeForBuild $ name ++ "-" ++ ver
             --io $ System.IO.hPutStrLn System.IO.stderr $ "copySource " ++ show debSource
             copy <- copyDebianSourceTree debSource (appendPath ("/" ++ newdir) dest)
             -- Clean the revision control files for this target out of the copy of the source tree
             cleanTarget target (topdir copy)
             findDebianBuildTree dest newdir >>= return . fromJust
      copyBuild :: DebianBuildTree -> AptIO DebianBuildTree
      copyBuild debBuild =
          do let name = logPackage . entry $ debBuild
                 dest = EnvPath (rootDir os) ("/work/build/" ++ name)
                 ver = Debian.Version.version . logVersion . entry $ debBuild
                 newdir = escapeForBuild $ name ++ "-" ++ ver
             --io $ System.IO.hPutStrLn System.IO.stderr $ "copyBuild " ++ show debBuild
             copy <- copyDebianBuildTree debBuild dest
             cleanTarget target (topdir copy)
             when (newdir /= (subdir debBuild))
                      (io $ renameDirectory (outsidePath dest ++ "/" ++ subdir debBuild) (outsidePath dest ++ "/" ++ newdir))
             findDebianBuildTree dest newdir >>= return . fromJust

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

{-
copy :: Target -> EnvPath -> AptIO DebianBuildTree
copy (Target (Tgt tgt) buildTree _ _ _ _) dest = prepareCopy tgt buildTree dest
-}

readSpec :: Bool -> FilePath -> Bool -> SourcesChangedAction -> [NamedSliceList] -> String -> AptIO Tgt
readSpec debug top flush ifSourcesChanged distros text =
    msgLn 0 (text ++ ":") >>
    setStyle (addPrefixes " " " ") readSpec'
    where
      readSpec' =
          case text of
            'a':'p':'t':':' : target -> prepareApt top flush ifSourcesChanged distros target
            'd':'a':'r':'c':'s':':' : target -> prepareDarcs debug top flush target
            'd':'e':'b':'-':'d':'i':'r':':' : target ->
                do (upstream, debian) <- parsePair debug target
                   prepareDebDir debug top flush upstream debian
            'd':'i':'r':':' : target -> prepareDir debug top flush (rootEnvPath target)
            'h':'g':':' : target -> prepareHg debug top flush target
            'q':'u':'i':'l':'t':':' : target ->
                do (base, patch) <- parsePair debug target
                   prepareQuilt top flush base patch
            's':'o':'u':'r':'c':'e':'d':'e':'b':':' : target ->
                readSpec debug top flush ifSourcesChanged distros target >>= prepareSourceDeb
            's':'v':'n':':' : target -> prepareSvn debug top flush target
            't':'l':'a':':' : target -> prepareTla top flush target
            'u':'r':'i':':' : target -> prepareUri debug top flush target
            'p':'r':'o':'c':':' : target ->
                do base <- readSpec debug top flush ifSourcesChanged distros target
                   prepareProc top flush base
            _ -> error ("Error in target specification: " ++ text)
      parsePair :: Bool -> String -> AptIO (Tgt, Tgt)
      parsePair debug text =
          case match "\\(([^)]*)\\):\\(([^)]*)\\)" text of
            Just [baseName, patchName] ->
                do a <- readSpec debug top flush ifSourcesChanged distros baseName
                   b <- readSpec debug top flush ifSourcesChanged distros patchName
                   return (a, b)
            _ -> error ("Invalid spec name: " ++ text)
      match = matchRegex . mkRegex

-- | Build a set of targets.  When a target build is successful it
-- is uploaded to the incoming directory of the local repository,
-- and then the function to process the incoming queue is called.
buildTargets :: (AptCache t) => Params.Params -> OSImage -> Relations -> LocalRepo -> t -> [Tgt] -> AptIO (LocalRepo, [Target])
buildTargets _ _ _ localRepo _ [] = return (localRepo, [])
buildTargets params cleanOS globalBuildDeps localRepo poolOS targetSpecs =
    do
      -- showTargets targetSpecs
      targetList <- prepareAllTargetSource cleanOS
      msgLn 0 "Building all targets:"
      failed <- setStyle (addPrefixes " " " ") $ buildLoop cleanOS globalBuildDeps (length targetList) (targetList, [])
      return (localRepo, failed)
      --buildAll cleanOS targetList globalBuildDeps
    where
      -- Retrieve and/or update the source code of all the targets before building.
      --prepareAllTargetSource :: OSImage -> AptIO [Target]
      prepareAllTargetSource cleanOS =
          do
            msgLn 0 "Assembling clean source tree for each target:"
            (debugStyle . iStyle) $ countAndPrepareTargets cleanOS targetSpecs
          where
            iStyle = setStyle (addPrefixes " " " ")
      -- Execute the target build loop until everything is built
      --buildLoop :: OSImage -> Relations -> Int -> ([Target], [Target]) -> AptIO [Target]
      buildLoop _ _ _ ([], failed) = return failed
      buildLoop cleanOS globalBuildDeps count (unbuilt, failed) =
          do
            targetGroups <- chooseNextTarget globalBuildDeps unbuilt
            vPutStrLn 1 ("\n\n" ++ makeTable targetGroups)
            case targetGroups of
              (group@(target : blocked) : other) ->
                  vBOL 0 >> vPutStr 0 (printf "[%2d of %2d] TARGET: %s\n"
                                       (count - length (concat targetGroups) + 1) count (show target)) >>
                  setStyle (addPrefixes " " " ") (buildTarget' target) >>=
                  either (\ e -> do vBOL 0
                                    vPutStrLn 0 "Package build failed:"
                                    indent "  " $ vPutStrLn 0 e
                                    vPutStrLn 0 ("Discarding " ++ show target ++ " and its dependencies:\n  " ++
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
                do vBOL 0
                   showElapsed "Total elapsed for target: " $ buildTarget params cleanOS globalBuildDeps localRepo poolOS target
      -- Find the sources.list for the distribution we will be building in.
      indent s = setStyle (addPrefix stderr s)
      debugStyle = setStyle (cond Debian.IO.dryRun Debian.IO.realRun (Params.debug params))

-- | It is not possible to precompute a build order for the targets,
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
--
-- The return value is a pair, the list of targets yet to be built and
-- the list of targets that failed to build.
chooseNextTarget :: Relations -> [Target] -> AptIO [[Target]]
chooseNextTarget globalBuildDeps targets =
    getDependencyInfo targets >>= return . buildGroups
    where
      getDependencyInfo :: [Target] -> AptIO [(Target, GenBuildDeps.DepInfo)]
      getDependencyInfo targets =
          mapM (getDepInfo globalBuildDeps . cleanSource) targets >>= return . (zip targets)
      -- Return the set of build dependency groups, each of which
      -- starts with a target that is ready to build followed by
      -- targets which are blocked by the first target.
      buildGroups :: [(Target, GenBuildDeps.DepInfo)] -> [[Target]]
      buildGroups pairs = map (map fst) (GenBuildDeps.buildable depends pairs)
      getDepInfo :: Relations -> DebianBuildTree -> AptIO GenBuildDeps.DepInfo
      getDepInfo globalBuildDeps buildTree =
          do
            --let sourceTree = debTree buildTree
            let controlPath = appendPath "/debian/control" (debdir buildTree)
            info <- io $ GenBuildDeps.genDep (outsidePath controlPath)
            -- My.ePutStr ("getDepInfo " ++ show target ++ ": " ++ show info)
            return $ addRelations globalBuildDeps info
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
    LocalRepo ->			-- The local repository the packages will be uploaded to
    t ->
    Target ->				-- what to build.  The first element is the original target,
					--   the second is the same with any source code control
					--   files cleaned out (so the .diff.gz will look good.)
    AptIO (Either String LocalRepo)	-- The local repository after the upload, or an error message
buildTarget params cleanOS globalBuildDeps repo poolOS target =
    do
      syncPool cleanOS
      -- Get the control file from the clean source and compute the
      -- build dependencies
      let debianControl = targetControl target
      msgLn 0 "Loading package lists and searching for build dependency solution..."
      solutions <- iStyle $ buildDepSolutions' (Params.preferred params) cleanOS globalBuildDeps debianControl
      case solutions of
        Left excuse -> do msgLn 0 ("Couldn't satisfy build dependencies\n " ++ excuse)
                          error "Couldn't satisfy build dependencies"
        Right [] -> error "Internal error"
        Right ((count, sourceDependencies) : _) ->
            do msgLn 0 ("Build dependency solution #" ++ show count ++ ": " ++ show sourceDependencies)
               -- Get the newest available version of a source package,
               -- along with its status, either Indep or All
               let (releaseControlInfo, releaseStatus, message) = getReleaseControlInfo cleanOS packageName
               msgLn 2 message
               msgLn 0 ("Status of " ++ packageName ++ maybe "" (\ p -> "-" ++ show (packageVersion . sourcePackageID $ p)) releaseControlInfo ++
                        ": " ++ explainSourcePackageStatus releaseStatus)
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
               sourceRevision <- case realSource target of (Tgt spec) -> BuildTarget.revision spec
               -- Get the changelog entry from the clean source
               let sourceLog = entry . cleanSource $ target
               let sourceVersion = logVersion sourceLog
               let sourcePackages = aptSourcePackagesSorted poolOS [packageName]
               let newVersion = computeNewVersion params sourcePackages releaseControlInfo sourceVersion
               msgLn 0 $ "Version number for new build: " ++ show sourceVersion ++ " -> " ++ show newVersion
               let ignoredBuildDeps = filterPairs (logPackage sourceLog) (Params.relaxDepends params)
               let decision =
                       buildDecision (realSource target) (Params.vendorTag params)
                                         (Params.forceBuild params) ignoredBuildDeps sourceLog
                                         oldVersion oldRevision oldDependencies releaseStatus
                                         sourceVersion sourceRevision sourceDependencies
               msgLn 0 ("Build decision: " ++ show decision)
               -- FIXME: incorporate the release status into the build decision
               case decision of
                 No _ -> return (Right repo)
                 Yes _ ->  buildPackage params cleanOS (Just newVersion) oldDependencies sourceRevision sourceDependencies target None repo sourceLog
                 Arch _ -> buildPackage params cleanOS oldVersion oldDependencies sourceRevision sourceDependencies target releaseStatus repo sourceLog
                 Auto _ -> buildPackage params cleanOS (Just newVersion) oldDependencies sourceRevision sourceDependencies target None repo sourceLog
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
      iStyle = setStyle (addPrefixes " " " ")

-- | Build a package and upload it to the local repository.
buildPackage :: Params.Params -> OSImage -> Maybe DebianVersion -> [PkgVersion] -> Maybe String -> [PkgVersion] -> Target -> SourcePackageStatus -> LocalRepo -> ChangeLogEntry -> AptIO (Either String LocalRepo)
buildPackage params cleanOS newVersion oldDependencies sourceRevision sourceDependencies target status repo sourceLog =
    do -- If this is a dry run we don't want to actually build any
       -- packages, so we are done.
       when (Params.dryRun params) 
                (do msgLn 0 "Not proceeding due to -n option."
                    io $ exitWith ExitSuccess)
       -- Prepare a copy of the source code in the build environment
       buildTree <- prepareBuildImage params cleanOS sourceDependencies buildOS target (Params.strictness params)

       -- If the noClean option is given we assume that a previous
       -- build has left the build environment ready for another run
       -- of dpkg-buildpackage -nc, so we mustn't sync the clean
       -- environment to the build environment.  It could be argued
       -- that we should still install the dependencies (which might
       -- have changed) but we currently are skipping that too.
       when (not (Params.noClean params))
            (do 
                
                -- Create the new changelog entry and add it to the file
                io $ maybeAddLogEntry buildTree newVersion)
       -- Build the binary debs
       result <- case realSource target of
                   Tgt t -> buildPkg (Params.noClean params) (Params.setEnv params) buildOS buildTree status t
       -- Upload the results to the local repository
       (changesFile :: Either String ChangesFile) <- io $ Debian.SourceTree.findChanges buildTree
       case result of
         (ExitSuccess, elapsed) -> either (return . Left) (doLocalUpload elapsed) changesFile
         (ExitFailure _, _) -> return . Left $ "Upload failed"
    where
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
          where setDist name (Field ("Distribution", _)) = Field ("Distribution", ' ' : relName name)
                setDist _ other = other
      doLocalUpload :: TimeDiff -> ChangesFile -> AptIO (Either String LocalRepo)
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
            uploadLocal repo changesFile'
            -- The upload to the local repository is done even when
            -- the --dry-run flag is given.  Or it would be if we
            -- didn't exit when the first buildworthy target is found.
            (_, errors) <- iStyle $ scanIncoming True Nothing repo
            case errors of
              [] -> return . Right $ repo
              _ -> return . Left $ "Local upload failed:\n" ++ showErrors (map snd errors)
      buildOS = Debian.OSImage.chrootEnv cleanOS (Params.dirtyRoot params)
      iStyle = setStyle (addPrefixes " " " ")

{-
withPreparedBuildImage :: BuildTarget t => OSImage -> [PkgVersion] -> OSImage -> Target -> Bool -> [String] -> Params.Strictness -> (t -> DebianBuildTree -> AptIO ()) -> AptIO ()
withPreparedBuildImage cleanOS sourceDependencies buildOS target@(Target (Tgt tgt) _ _ _ _ _) debug buildDepends strictness f =
    case strictness of
      Params.Lax ->
          do iStyle $ installDependencies cleanOS (cleanSource target) buildDepends sourceDependencies
             syncStyle $ OSImage.syncEnv cleanOS buildOS
             debugStyle $ prepareCopy tgt (cleanSource target) newPath
             f tgt buildTree
      _ ->
          do syncStyle $ OSImage.syncEnv cleanOS buildOS
             buildTree <- debugStyle $ prepareCopy tgt (cleanSource target) newPath
             iStyle $ installDependencies buildOS buildTree buildDepends sourceDependencies
             f tgt buildTree
-}

-- |Prepare the build image by copying the clean image, installing
-- dependencies, and copying the clean source tree.  For a lax build
-- these operations take place in a different order from other types
-- of builds.  For lax: dependencies, then image copy, then source
-- copy.  For other: image copy, then source copy, then dependencies.
prepareBuildImage :: Params.Params -> OSImage -> [PkgVersion] -> OSImage -> Target -> Params.Strictness -> AptIO DebianBuildTree
prepareBuildImage params cleanOS sourceDependencies buildOS target@(Target (Tgt tgt) _ _ _ _ _) Params.Lax =
    do
      iStyle $ installDependencies cleanOS (cleanSource target) buildDepends sourceDependencies
      case noClean of
        False -> do syncStyle $ Debian.OSImage.syncEnv cleanOS buildOS
                    debugStyle $ prepareCopy tgt (cleanSource target) newPath
        True -> findOneDebianBuildTree newPath >>= return . maybe (error $ "No build tree at " ++ show newPath) id
    where
      debug = (Params.debug params)
      buildDepends = (Params.buildDepends params)
      noClean = Params.noClean params
      newPath = EnvPath {envRoot = rootDir buildOS, envPath = envPath oldPath}
      oldPath = topdir . cleanSource $ target
      syncStyle = setStyle (setStart (Just "Syncing buildOS"))
      iStyle = setStyle (addPrefixes " " " ")
      debugStyle = setStyle (cond Debian.IO.dryRun Debian.IO.realRun debug)
prepareBuildImage params cleanOS sourceDependencies buildOS target@(Target (Tgt tgt) _ _ _ _ _) _ =
    do
      buildTree <- 
          case noClean of
            False -> debugStyle $ prepareCopy tgt (cleanSource target) newPath
            True -> findOneDebianBuildTree newPath >>= maybe (error "build tree not found") return
      iStyle $ downloadDependencies cleanOS buildTree buildDepends sourceDependencies
      case noClean of
        False -> syncStyle $ Debian.OSImage.syncEnv cleanOS buildOS
        True -> return buildOS
      iStyle $ installDependencies buildOS buildTree buildDepends sourceDependencies
      return buildTree
    where
      debug = Params.debug params
      buildDepends = Params.buildDepends params
      noClean = Params.noClean params
      newPath = EnvPath {envRoot = rootDir buildOS, envPath = (envPath . topdir . cleanSource $ target)}
      syncStyle = setStyle (setStart (Just "Syncing buildOS"))
      iStyle = setStyle (addPrefixes " " " ")
      debugStyle = setStyle (cond Debian.IO.dryRun Debian.IO.realRun debug)
            
            

-- | This contains information fully identifying a package's build
-- environment: a string from the source code control system
-- identifying the exact version of the package's source code, and all
-- names and version number of the package's build dependencies.
-- This information used to decide whether to rebuild.
{-
data RevInfo = RevInfo { revVersion :: Maybe DebianVersion,
                         depVersions :: [PkgVersion],
                         revString :: Maybe String }
-}

-- | Get the control info for the newest version of a source package
-- available in a release.  Make sure that the files for this build
-- architecture are available.
getReleaseControlInfo :: OSImage -> String -> (Maybe (SourcePackage Repository), SourcePackageStatus, String)
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
      isComplete :: Repo r => [BinaryPackage r] -> (SourcePackage r, [String]) -> Bool
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
      availableDebNames :: Repo r => [BinaryPackage r] -> SourcePackage r -> [String]
      availableDebNames binaryPackages sourcePackage =
          map fst . map binaryPackageVersion . filter checkSourceVersion $ binaryPackages
          where checkSourceVersion binaryPackage = maybe False ((==) sourceVersion) (binaryPackageSourceVersion binaryPackage)
                sourceVersion = sourcePackageVersion sourcePackage
      --  or (if it is a udeb) if it simply exists on the
      -- server and has the correct filename.  There is no way to
      -- decide whether a package is a udeb from the package indexes.
      unableToCheckUDebs = True
      availableUDebNames :: Repo r => SourcePackage r -> [String]
      availableUDebNames _sourcePackage = undefined

-- | Get the "old" package version number, the one that was already
-- built and uploaded to the repository.
getOldVersion :: Repo r => SourcePackage r -> DebianVersion
getOldVersion package = packageVersion . sourcePackageID $ package

getOldRevision :: Repo r => SourcePackage r -> (Maybe String, [PkgVersion])
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
computeNewVersion :: Repo r => Params.Params -> [SourcePackage r] -> Maybe (SourcePackage r) -> DebianVersion -> DebianVersion
computeNewVersion params
                  available		-- All the versions that exist in the pool in any dist,
					-- the new version number must not equal any of these.
                  current		-- The control file paragraph for the currently uploaded
                                        -- version in this dist.  The new version must be newer
                                        -- than this.
                  sourceVersion =	-- Version number in the changelog entry of the checked-out
                                        -- source code.  The new version must also be newer than this.
    case Params.doNotChangeVersion params of
      True -> sourceVersion
      False ->
          let vendor = Params.vendorTag params
              release = if (Params.isDevelopmentRelease params) then
                            Nothing else
                            (Just (sliceName (Params.baseRelease params)))
              extra = Params.extraReleaseTag params in
          setTag vendor release extra currentVersion (catMaybes . map getVersion $ available) sourceVersion
    where
      getVersion paragraph =
          maybe Nothing (Just . parseDebianVersion . B.unpack) (fieldValue "Version" . sourceParagraph $ paragraph)
      currentVersion =
          maybe Nothing (Just . parseDebianVersion . B.unpack) (maybe Nothing (fieldValue "Version" . sourceParagraph) current)

buildDepSolutions' :: [String] -> OSImage -> Relations -> Control -> AptIO (Either String [(Int, [PkgVersion])])
buildDepSolutions' preferred os globalBuildDeps debianControl =
    do
      arch <- io $ buildArchOfEnv (rootDir os)
      let (_, relations, _) = GenBuildDeps.buildDependencies debianControl
      let relations' = filterRelations arch (relations ++ globalBuildDeps)
      vBOL 1 >> vPutStrLn 1 ("Build dependency relations:\n  " ++
                             concat (intersperse "\n  " (map show relations')))
      let relations'' = computeBuildDeps os arch relations'
      vPutStrLn 1 $ ("Build dependency relations with virtual packages replaced:\n  " ++
                     concat (intersperse "\n  " (map show relations'')))
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

-- This is a copy of a function Jeremy made private in Debian.IO.
-- This probably means there is a standard replacement for it - must
-- find out.
myTimeDiffToString diff =
    do
      case () of
        _ | isPrefixOf "00:00:0" s -> drop 7 s ++ printf ".%03d" ms ++ " s."
        _ | isPrefixOf "00:00:" s -> drop 6 s ++ printf ".%03d" ms ++ " s."
        _ | isPrefixOf "00:" s -> drop 3 s
        _ -> s
    where
      s = formatTimeDiff defaultTimeLocale "%T" diff
      ms = ps2ms ps
      ps2ms ps = quot (ps + 500000000) 1000000000
      ps = tdPicosec diff

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
downloadDependencies :: OSImage -> DebianBuildTree -> [String] -> [PkgVersion] -> AptIO TaskSuccess_
downloadDependencies os source extra versions =
    do vers <- liftIO (evaluate versions)
       vPutStrLn 1 . ("versions: " ++) . show $! vers
       builddepStyle $ systemTask_ ("chroot " ++ rootPath root ++ " bash -c " ++
                                    "\"export DEBIAN_FRONTEND=noninteractive; unset LANG; " ++
                                    (if True then aptGetCommand else pbuilderCommand) ++ "\"")
    where
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes install --download-only " ++ consperse " " (map showPkgVersion versions ++ extra)
      builddepStyle = setStyle (setStart (Just ("Downloading build dependencies into " ++ show (rootDir os))) .
                                addPrefixes " " " " .
                                setError (Just "Could not satisfy build dependencies."))
      path = envPath (topdir source)
      root = rootDir os

-- |Install the package's build dependencies.
installDependencies :: OSImage -> DebianBuildTree -> [String] -> [PkgVersion] -> AptIO TaskSuccess_
installDependencies os source extra versions =
    builddepStyle $ systemTask_ ("chroot " ++ rootPath root ++ " bash -c " ++
                                 "\"export DEBIAN_FRONTEND=noninteractive; unset LANG; " ++
                                 (if True then aptGetCommand else pbuilderCommand) ++ "\"")
    where
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes install " ++ consperse " " (map showPkgVersion versions ++ extra)
      builddepStyle = setStyle (setStart (Just ("Installing build dependencies into " ++ show (rootDir os))) .
                                addPrefixes " " " " .
                                setError (Just "Could not satisfy build dependencies."))
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
      _ | sourceVersion > fromJust oldVersion ->
            Yes ("Source version (" ++ show sourceVersion ++ ") " ++
                 "is newer than release version (" ++ maybe "Nothing" show oldVersion ++ ")")
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
