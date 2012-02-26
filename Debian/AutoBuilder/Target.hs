-- |A Target represents a particular set of source code and the
-- methods to retrieve and update it.
-- 
-- Author: David Fox <ddssff@gmail.com>
{-# LANGUAGE PackageImports, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS -Wall -Werror -fwarn-unused-imports -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.AutoBuilder.Target
    ( changelogText	-- Tgt -> Maybe String -> [PkgVersion] -> String
    , buildTargets
    , showTargets 
    ) where

import Control.Arrow (second)
import Control.Applicative.Error (Failing(..), failing)
import Control.Exception (Exception(..), SomeException, try, evaluate)
import Control.Monad.RWS(MonadIO(..), MonadTrans(..), when)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Either (partitionEithers)
import Data.List(intersperse, find, intercalate, intersect, isSuffixOf,
                 nub, partition, sortBy)
import qualified Data.Map as Map
import Data.Maybe(catMaybes, fromJust, isJust, isNothing, listToMaybe)
import qualified Data.Set as Set
import Data.Time(NominalDiffTime)
import Debian.AutoBuilder.BuildTarget.Common (BuildTarget(logText, buildWrapper))
import qualified Debian.AutoBuilder.BuildTarget.Common as BuildTarget
import qualified Debian.AutoBuilder.BuildTarget.Proc as Proc
import qualified Debian.AutoBuilder.Params as P
import qualified Debian.AutoBuilder.Types.CacheRec as P
import Debian.AutoBuilder.TargetType (Target(tgt, cleanSource), targetName, prepareTarget, targetRelaxed, targetControl)
import Debian.AutoBuilder.Tgt (Tgt)
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Version as V
import Debian.Changes (prettyChanges, ChangesFile(changeRelease, changeInfo, changeFiles, changeDir),
                       ChangedFileSpec(changedFileSize, changedFileName, changedFileMD5sum, changedFileSHA1sum, changedFileSHA256sum),
                       ChangeLogEntry(logWho, logVersion, logDists, logDate, logComments))
import Debian.Control
import qualified Debian.Control.String as S(fieldValue)
import qualified Debian.GenBuildDeps as G
import Debian.Relation (prettyRelation)
import Debian.Relation.ByteString(Relations, Relation(..))
import Debian.Release (releaseName')
import Debian.Repo.SourceTree (buildDebs)
import Debian.Sources (SliceName(..))
import Debian.Repo (chrootEnv, syncEnv, syncPool, updateEnv)
import Debian.Repo.Cache (binaryPackages, buildArchOfEnv, sourcePackages, aptSourcePackagesSorted)
import Debian.Repo.Dependencies (simplifyRelations, solutions)
import Debian.Repo.Changes (save, uploadLocal)
import Debian.Repo.Insert (scanIncoming, showErrors)
import Debian.Repo.OSImage (OSImage, updateLists)
import Debian.Repo.Package (binaryPackageSourceVersion, sourcePackageBinaryNames)
import Debian.Repo.Repository (readPkgVersion, showPkgVersion)
import Debian.Repo.SourceTree (SourceTreeC(..), DebianSourceTreeC(..),
                               DebianBuildTree, addLogEntry, copyDebianBuildTree,
                               findChanges, findOneDebianBuildTree, SourcePackageStatus(..))
import Debian.Repo.Monad (AptIOT)
import Debian.Repo.Types (SourcePackage(sourceParagraph, sourcePackageID),
                          AptCache(rootDir, aptBinaryPackages), EnvRoot(rootPath),
                          PackageID(packageVersion, packageName), LocalRepository, PkgVersion(..),
                          BinaryPackage(packageInfo, packageID), prettyPkgVersion)
import Debian.Time(getCurrentLocalRFC822Time)
import Debian.Version(DebianVersion, parseDebianVersion, prettyDebianVersion)
import Debian.VersionPolicy(dropTag, parseTag, setTag)
import System.Unix.Process(Output(..), collectOutputUnpacked, mergeToStdout, lazyProcess, stdoutOnly)
import Extra.Files(replaceFile)
import "Extra" Extra.List(dropPrefix)
import Extra.Misc(columns)
import System.Directory (doesFileExist, doesDirectoryExist, removeDirectory, createDirectoryIfMissing)
import System.Exit(ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Files(fileSize, getFileStatus)
import System.Unix.Chroot (useEnv, forceList)
import System.Unix.Process (collectResult, exitCodeOnly)
import System.Unix.Progress (lazyCommandF, lazyCommandE, lazyCommandV, lazyProcessF)
import System.Unix.QIO (quieter, quieter', qPutStrLn, qMessage, q12 {-, q02-})
import Text.PrettyPrint (Doc, text, cat)
import Text.Printf(printf)
import Text.Regex(matchRegex, mkRegex)
{-
deriving instance Show VersionReq
deriving instance Show ArchitectureReq
deriving instance Show Relation
-}

--liftTIO = lift

prettySimpleRelation :: Maybe PkgVersion -> Doc
prettySimpleRelation rel = maybe (text "Nothing") (\ v -> cat [text (getName v ++ "="), prettyDebianVersion (getVersion v)]) rel

{-
_findSourceParagraph (Control paragraphs) = 
    case dropWhile isCommentParagraph paragraphs of
      (paragraph : _) -> Just paragraph
      _ -> Nothing
    where
      isCommentParagraph (Paragraph fields) = all isCommentField fields
      isCommentField (Comment _) = True
      isCommentField _ = False
-}

-- |Generate the details section of the package's new changelog entry
-- based on the target type and version info.  This includes the
-- revision info and build dependency versions in a human readable
-- form.  FIXME: this should also include revision control log
-- entries.
changelogText :: Exception e => Tgt -> Either e String -> [PkgVersion] -> [PkgVersion] -> String
changelogText spec revision oldDeps newDeps =
    ("  * " ++ logText spec revision ++ "\n" ++ depChanges changedDeps ++ "\n")
    where
      depChanges [] = ""
      depChanges _ = "  * Build dependency changes:" ++ prefix ++ intercalate prefix padded ++ "\n"
      padded = map concat . columns . map showDepChange $ changedDeps
      changedDeps = Set.toList (Set.difference (Set.fromList newDeps) (Set.fromList oldDeps))
      showDepChange newDep =
          case filter (hasName (getName newDep)) oldDeps of
            [] -> [" " ++ getName newDep ++ ": ", "(none)", " -> ", show (prettyDebianVersion (getVersion newDep))]
            (oldDep : _) -> [" " ++ getName newDep ++ ": ", show (prettyDebianVersion (getVersion oldDep)), " -> ", show (prettyDebianVersion (getVersion newDep))]
      hasName name deps = ((== name) . getName) deps
      prefix = "\n    "

-- |Generate the string of build dependency versions:
-- package1=version1 package2=version2 ...
_formatVersions :: [PkgVersion] -> String
_formatVersions buildDeps =
    -- "\n  * Build dependency versions:" ++
    prefix ++
    intercalate prefix (map (show . prettyPkgVersion) buildDeps) ++
    "\n"
    where prefix = "\n    "

--  (P.debug params) (P.topDir params) (P.flushSource params) (P.ifSourcesChanged params) (P.allSources params)

prepareTargets :: P.CacheRec -> OSImage -> Relations -> [(String, Tgt)] -> AptIOT IO [Target]
prepareTargets cache cleanOS globalBuildDeps targetSpecs =
    do -- showTargets targetSpecs
       results <- lift $ mapM (prepare (length targetSpecs)) (zip [1..] targetSpecs)
       let (failures, targets) = partitionEithers results
       when (not (null failures))
                (do let msg = intercalate "\n " (("Could not prepare " ++ show (length failures) ++ " targets:") : map (show . toException) failures)
                    liftIO $ hPutStrLn stderr msg
                    error msg)
       return targets
    where
      prepare :: Int -> (Int, (String, Tgt)) -> IO (Either SomeException Target)
      prepare count (index, (name, tgt)) =
          do qPutStrLn (printf "[%2d of %2d] %s" index count (show tgt))
             result <- quieter' (+ 2) (try' (prepareTarget cache globalBuildDeps cleanOS name tgt))
             either (\ e -> do hPutStrLn stderr (printf "[%2d of %2d] - could not prepare %s: %s" index count name (show e))
                               return (Left e))
                    (return . Right) result
      try' :: IO a -> IO (Either SomeException a)
      try' = try

-- | Build a set of targets.  When a target build is successful it
-- is uploaded to the incoming directory of the local repository,
-- and then the function to process the incoming queue is called.
buildTargets :: (AptCache t) => P.CacheRec -> OSImage -> Relations -> LocalRepository -> t -> [(String, Tgt)] -> AptIOT IO (LocalRepository, [Target])
buildTargets _ _ _ localRepo _ [] = return (localRepo, [])
buildTargets cache cleanOS globalBuildDeps localRepo poolOS targetSpecs =
    do
      qPutStrLn "\nAssembling source trees:\n"
      targets <- prepareTargets cache cleanOS globalBuildDeps targetSpecs
      qPutStrLn "\nBuilding all targets:"
      failed <- buildLoop cache globalBuildDeps localRepo poolOS cleanOS targets
      return (localRepo, failed)
    where
      -- targetList <- lift $ countAndPrepareTargets cache globalBuildDeps cleanOS targetSpecs
      --buildAll cleanOS targetList globalBuildDeps

-- Execute the target build loop until all the goals (or everything) is built
buildLoop :: (AptCache t) => P.CacheRec -> Relations -> LocalRepository -> t -> OSImage -> [Target] -> AptIOT IO [Target]
buildLoop cache globalBuildDeps localRepo poolOS cleanOS' targets =
    loop cleanOS' (length targets) (targets, [])
    where
      loop _ _ ([], failed) = return failed
      loop cleanOS' count (unbuilt, failed) =
         do -- relaxed <- lift $ updateDependencyInfo (P.relaxDepends (P.params cache)) globalBuildDeps unbuilt
            next <- lift $ chooseNextTarget cache (goals unbuilt) unbuilt
            case next of
              Nothing -> return failed
              Just (target, blocked, other) ->
                  do quieter' (const 0) (qPutStrLn (printf "[%2d of %2d] TARGET: %s - %s"
                                               (count - length unbuilt + 1) count (targetName target) (show (tgt target))))
                     result <- if Set.member (targetName target) (P.discard (P.params cache))
                               then return (Failure ["--discard option set"])
                               else buildTarget cache cleanOS' globalBuildDeps localRepo poolOS target
                     failing (\ errs ->
                                  do quieter (const 0) $
                                       qPutStrLn ("Package build failed:\n " ++ intercalate "\n " errs ++ "\n" ++
                                                  "Discarding " ++ targetName target ++ " and its dependencies:\n  " ++
                                                  concat (intersperse "\n  " (map targetName blocked)))
                                     loop cleanOS' count (other, (target : blocked) ++ failed))
                             (\ mRepo ->
                                  do cleanOS'' <- maybe (return cleanOS')
                                                       (\ _ -> updateEnv cleanOS' >>= either (\ e -> error ("Failed to update clean OS:\n " ++ show e)) return)
                                                       mRepo
                                     loop cleanOS'' count (blocked ++ other, failed))
                             result

      -- If no goals are given in the build parameters, assume all
      -- known targets are goals.
      goals targets =
          case P.goals (P.params cache) of
            [] -> targets
            goalNames -> filter (\ target -> elem (targetName target) goalNames) targets

      -- Find the sources.list for the distribution we will be building in.
      --indent s = setStyle (addPrefix stderr s)
      --debugStyle = setStyle (cond Debian.IO.dryRun Debian.IO.realRun (P.debug params))

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
chooseNextTarget :: P.CacheRec -> [Target] -> [Target] -> IO (Maybe (Target, [Target], [Target]))
chooseNextTarget _ [] _ = return Nothing
chooseNextTarget cache goals targets =
    q12 "Choosing next target" $
    -- Compute the list of build dependency groups, each of which
    -- starts with a target that is ready to build followed by
    -- targets which are blocked by the first target.
    case G.buildable depends targets of
      (G.CycleInfo arcs) -> error (cycleMessage cache arcs)
      info ->
          do quieter (\x->x-2) $ qPutStrLn (makeTable info)
             return . listToMaybe . sortBy (compareReady goals) . G.readyTriples $ info
    where
      makeTable (G.BuildableInfo ready _other) =
          unlines . map (intercalate " ") . columns $ goalsLine ++ readyLines
          where
            goalsLine = [{- [" Goals: ", "[" ++ intercalate ", " (map targetName goals) ++ "]"] -}]
            readyLines = map readyLine ready
            readyLine (ready', blocked, _other) = 
                [" Ready:", targetName ready', "Blocking: [" ++ intercalate ", " (map targetName blocked) ++ "]"]
      makeTable (G.CycleInfo pairs) =
          error $ "Cycle detected by Debian.GenBuildDeps.buildable: " ++ show (map (\ (a, b) -> (tgt a, tgt b)) pairs)
      -- We choose the next target using the relaxed dependency set
      depends :: Target -> Target -> Ordering
      depends target1 target2 = G.compareSource (targetRelaxed (P.relaxDepends (P.params cache)) target1) (targetRelaxed (P.relaxDepends (P.params cache)) target2)
      -- Choose the next target to build.  Look for targets which are
      -- in the goal list, or which block packages in the goal list.
      -- Among those, prefer the target which blocks the most
      -- packages.  If there are goal targets but none of them are
      -- ready to build or directly block 
      -- targets include a goal as readyamongoals none of the 
      compareReady :: [Target] -> (Target, [Target], [Target]) ->  (Target, [Target], [Target]) -> Ordering
      compareReady goals' (aReady, aBlocked, _) (bReady, bBlocked, _) =
          -- Prefer targets which include a goal
          case compare (length bGoals) (length aGoals) of
            -- Otherwise, prefer the target which blocks the most other targets
            EQ -> compare (length bBlocked) (length aBlocked)
            x -> x
          where 
            aGoals = intersect goals' (aReady : aBlocked)
            bGoals = intersect goals' (bReady : bBlocked)

cycleMessage :: P.CacheRec -> [(Target, Target)] -> String
cycleMessage cache arcs =
    "Dependency cycles formed by these edges need to be broken:\n  " ++
    unlines (map (intercalate " ")
             (columns (["these binary packages", "from this source package", "", "force a rebuild of"] :
                       (map arcTuple arcs)))) ++
    "\nAdd one or more of these lines (but as few as possible) to your configuration file:\n  " ++
    intercalate "\n  " (map relaxLine (nub (concat (map pairs arcs))))
    where
      arcTuple (pkg, dep) = 
          let rels = targetRelaxed (P.relaxDepends (P.params cache)) pkg in
          [(show (intersect (binaryNames dep) (binaryNamesOfRelations rels))), targetName dep, " -> ", targetName pkg]
      binaryNames dep =
          map (\ (G.BinPkgName name) -> name) xs
          where (_, _, xs) = (targetRelaxed (P.relaxDepends (P.params cache)) dep)
      relaxLine (bin, src) = "Relax-Depends: " ++ bin ++ " " ++ src
      pairs (pkg, dep) =
          map (\ bin -> (bin, targetName pkg)) binaryDependencies
              where binaryDependencies = intersect (binaryNames dep) (binaryNamesOfRelations (targetRelaxed (P.relaxDepends (P.params cache)) pkg))
      binaryNamesOfRelations (_, rels, _) =
          concat (map (map (\ (Rel name _ _) -> name)) rels)

showTargets :: P.Packages -> String
showTargets targets =
    unlines (heading :
             map (const '-') heading :
             map concat (columns (reverse (snd (P.foldPackages (\ spec _flags (count, rows) -> (count + 1, [printf "%4d. " count, P.srcPkgName spec, " ", show spec] : rows)) (1 :: Int, []) targets)))))
{-
    unlines (heading :
             map (const '-') heading :
             map concat (columns (map (\ (n, t) -> [printf "%4d. " n, P.name t, " ", show (P.spec t)]) pairs))) ++ "\n"
-}
    where
      -- pairs = zip [1..] targets :: [(Int, P.Packages)]
      heading = show (P.packageCount targets) ++ " Targets:"

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
    P.CacheRec ->			-- configuration info
    OSImage ->				-- cleanOS
    Relations ->			-- The build-essential relations
    LocalRepository ->			-- The local repository the packages will be uploaded to
    t ->
    Target ->
    AptIOT IO (Failing (Maybe LocalRepository))	-- The local repository after the upload (if it changed), or an error message
buildTarget cache cleanOS globalBuildDeps repo poolOS target =
    do
      _cleanOS' <- lift (quieter (+ 2) $ syncPool cleanOS)
      -- Get the control file from the clean source and compute the
      -- build dependencies
      let debianControl = targetControl target
      solns <- liftIO $ buildDepSolutions' (P.preferred (P.params cache)) cleanOS globalBuildDeps debianControl
      case solns of
        Failure excuses -> do let excuses' = ("Couldn't satisfy build dependencies" : excuses)
                              qPutStrLn (intercalate "\n " excuses')
                              return $ Failure excuses'
        Success [] -> error "Internal error 4"
        Success ((_count, sourceDependencies) : _) ->
            do let sourceDependencies' = map makeVersion sourceDependencies
               -- qPutStrLn (intercalate "\n  " (("Using build dependency solution #" ++ show count) : map show sourceDependencies'))
               -- Get the newest available version of a source package,
               -- along with its status, either Indep or All
               let (releaseControlInfo, releaseStatus, _message) = getReleaseControlInfo cleanOS packageName
               -- quieter (+ 1) (qPutStrLn message)
               -- qPutStrLn ("Status of " ++ packageName ++ maybe "" (\ p -> "-" ++ show (packageVersion . sourcePackageID $ p)) releaseControlInfo ++ ": " ++ explainSourcePackageStatus releaseStatus)
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
               sourceRevision <- lift (try (BuildTarget.revision (P.params cache) (tgt target)))
               -- Get the changelog entry from the clean source
               let sourceLog = entry . cleanSource $ target
               let sourceVersion = logVersion sourceLog
               -- qPutStrLn ("Released source version: " ++ show oldSrcVersion)
               -- qPutStrLn ("Released version: " ++ show oldVersion)
               -- qPutStrLn ("Current source version: " ++ show sourceVersion)
               let spkgs = aptSourcePackagesSorted poolOS [packageName]
                   buildTrumped = elem packageName (P.buildTrumped (P.params cache))
                   newVersion = computeNewVersion cache spkgs (if buildTrumped then Nothing else releaseControlInfo) sourceVersion
                   decision =
                       buildDecision cache target
                                         oldVersion oldSrcVersion oldRevision oldDependencies releaseStatus
                                         sourceVersion sourceDependencies'
               quieter (const 0) $ qPutStrLn ("Build decision: " ++ show decision)
               -- FIXME: incorporate the release status into the build decision
               case newVersion of
                 Failure messages ->
                    return (Failure messages)
                 Success version ->
                    case decision of
                      Error message -> return (Failure [message])
                      No _ -> return (Success Nothing)
                      Yes _ ->  buildPackage cache cleanOS (Just version) oldDependencies sourceRevision sourceDependencies' target None repo sourceLog >>= fm
                      Arch _ -> buildPackage cache cleanOS oldVersion oldDependencies sourceRevision sourceDependencies' target releaseStatus repo sourceLog >>= fm
                      Auto _ -> buildPackage cache cleanOS (Just version) oldDependencies sourceRevision sourceDependencies' target None repo sourceLog >>= fm
    where
      fm :: Monad m => Failing x -> m (Failing (Maybe x))
      fm (Failure msgs) = return (Failure msgs)
      fm (Success x) = return (Success (Just x))
      --buildTree = maybe (error $ "Invalid target for build: " ++ show target) id (getBuildTree . cleanSource $ target)
      packageName = targetName target
      -- Find or create an apt-get environment that will see all the packages
      -- in both the upload repository and the local repository, and then use
      -- it to compute a list of all existing versions of the package.
{-    compareVersion a b = case (fieldValue "Version" a, fieldValue "Version" b) of
                             (Just a', Just b') -> compare (parseDebianVersion a') (parseDebianVersion b')
                             _ -> error "Missing Version field" -}

-- appPrefix _ = id

-- |Convert to a simple name and version record to interface with older
-- code.
makeVersion :: BinaryPackage -> PkgVersion
makeVersion package =
    PkgVersion { getName = packageName (packageID package)
               , getVersion = packageVersion (packageID package) }

-- | Build a package and upload it to the local repository.
buildPackage :: P.CacheRec -> OSImage -> Maybe DebianVersion -> [PkgVersion] -> Either SomeException String -> [PkgVersion] -> Target -> SourcePackageStatus -> LocalRepository -> ChangeLogEntry -> AptIOT IO (Failing LocalRepository)
buildPackage cache cleanOS newVersion oldDependencies sourceRevision sourceDependencies target status repo sourceLog =
    checkDryRun >>
    lift prepareImage >>=
    failing (return . Failure) logEntry >>=
    failing (return . Failure) (quieter (+ (-1)) . build) >>=
    failing (return . Failure) find >>=
    failing (return . Failure) upload
    where
      checkDryRun = when (P.dryRun (P.params cache))
                      (do qPutStrLn "Not proceeding due to -n option."
                          liftIO (exitWith ExitSuccess))
      prepareImage = prepareBuildImage cache cleanOS sourceDependencies buildOS target (P.strictness (P.params cache))
      logEntry buildTree = 
          case P.noClean (P.params cache) of
            False -> liftIO $ maybeAddLogEntry buildTree newVersion >> return (Success buildTree)
            True -> return (Success buildTree)
      build :: DebianBuildTree -> AptIOT IO (Failing (DebianBuildTree, NominalDiffTime))
      build buildTree =
          do -- The --commit flag does not appear until dpkg-dev-1.16.1,
             -- so we need to check this version number.  We also
             -- don't want to leave the patches subdirectory here
             -- unless we actually created a patch.
             _ <- liftIO $ useEnv' root (\ _ -> return ())
                             (-- Get the version number of dpkg-dev in the build environment
                              lazyCommandF ("dpkg -s dpkg-dev | sed -n 's/^Version: //p'") L.empty >>= return . head . words . L.unpack . stdoutOnly >>= \ installed ->
                              -- If it is >= 1.16.1 we may need to run dpkg-source --commit.
                              lazyCommandV ("dpkg --compare-versions '" ++ installed ++ "' ge 1.16.1") L.empty >>= return . (== ExitSuccess) . exitCodeOnly >>= \ newer ->
                              when newer (doesDirectoryExist (path' </> "debian/patches") >>= doDpkgSource)
                              {- when newer (do createDirectoryIfMissing True (path' </> "debian/patches")
                                             -- Create the patch if there are any changes
                                             _ <- lazyProcessF "dpkg-source" ["--commit", ".", "autobuilder.diff"] (Just path') Nothing L.empty
                                             -- If the patch was not created, remove the directory
                                             exists <- doesFileExist (path' </> "debian/patches/autobuilder.diff")
                                             when (not exists) (removeDirectory (path' </> "debian/patches"))) -}
                             )
             result <- liftIO $ try (buildWrapper (P.params cache) buildOS buildTree status (tgt target)
                                     (buildDebs (P.noClean (P.params cache)) False (P.setEnv (P.params cache)) buildOS buildTree status))
             case result of
               Left (e :: SomeException) -> return (Failure [show e])
               Right elapsed -> return (Success (buildTree, elapsed))
          where
            doDpkgSource False =
                createDirectoryIfMissing True (path' </> "debian/patches") >>
                doDpkgSource' >>
                doesFileExist (path' </> "debian/patches/autobuilder.diff") >>= \ exists ->
                when (not exists) (removeDirectory (path' </> "debian/patches"))
            doDpkgSource True =
                doDpkgSource' >>
                return ()
            doDpkgSource' = lazyProcessF "dpkg-source" ["--commit", ".", "autobuilder.diff"] (Just path') Nothing L.empty
            path' = fromJust (dropPrefix root path)
            path = debdir buildTree
            root = rootPath (rootDir buildOS)
      find (buildTree, elapsed) =
          liftIO $ try (findChanges buildTree) >>=
                   return . either (\ (e :: SomeException) -> Failure [show e]) (\ changesFile -> Success (changesFile, elapsed))
      upload :: (ChangesFile, NominalDiffTime) -> AptIOT IO (Failing LocalRepository)
      upload (changesFile, elapsed) = doLocalUpload elapsed changesFile
      -- Depending on the strictness, build dependencies either
      -- get installed into the clean or the build environment.
      maybeAddLogEntry _ Nothing = return ()
      maybeAddLogEntry buildTree (Just newVersion) = makeLogEntry newVersion >>= (flip addLogEntry) buildTree
      makeLogEntry newVersion = 
          do
            date <- getCurrentLocalRFC822Time
            return $ sourceLog {logVersion=newVersion,
                                logDists=[P.buildRelease (P.params cache)],
                                logWho=P.autobuilderEmail (P.params cache),
                                logDate=date,
                                logComments=
                                    init (logComments sourceLog) ++ changelogText (tgt target) sourceRevision oldDependencies sourceDependencies}
      setDistribution name changes =
          let (Paragraph fields) = changeInfo changes in
          let info' = map (setDist name) fields in
          changes { changeInfo = Paragraph info'
                  , changeRelease = name }
          where setDist name (Field ("Distribution", _)) = Field ("Distribution", ' ' : releaseName' name)
                setDist _ other = other
      doLocalUpload :: NominalDiffTime -> ChangesFile -> AptIOT IO (Failing LocalRepository)
      doLocalUpload elapsed changesFile =
          do
            (changesFile' :: ChangesFile) <-
		-- Set the Distribution field in the .changes file to the one
                -- specified by the autobuilder Build-Release parameter.
                return (setDistribution (P.buildRelease (P.params cache)) changesFile) >>=
                -- Insert information about the build into the .changes file.
                liftIO . updateChangesFile elapsed >>=
                -- Insert the revision info into the .dsc file and update
                -- the md5sum of the .dsc file in the .changes file.
                liftIO . setRevisionInfo (logVersion sourceLog) sourceRevision sourceDependencies
            -- Upload to the local apt repository
            lift $ uploadLocal repo changesFile'
            -- The upload to the local repository is done even when
            -- the --dry-run flag is given.  Or it would be if we
            -- didn't exit when the first buildworthy target is found.
            (_, errors) <- scanIncoming True Nothing repo
            case errors of
              -- Update lists to reflect the availability of the package we just built
              [] -> liftIO (updateLists cleanOS) >> return (Success repo)
              _ -> return (Failure ["Local upload failed:\n " ++ showErrors (map snd errors)])
      buildOS = Debian.Repo.chrootEnv cleanOS (P.dirtyRoot cache)

-- |Prepare the build image by copying the clean image, installing
-- dependencies, and copying the clean source tree.  For a lax build
-- these operations take place in a different order from other types
-- of builds.  For lax: dependencies, then image copy, then source
-- copy.  For other: image copy, then source copy, then dependencies.
prepareBuildImage :: P.CacheRec -> OSImage -> [PkgVersion] -> OSImage -> Target -> P.Strictness -> IO (Failing DebianBuildTree)
prepareBuildImage cache cleanOS sourceDependencies buildOS target P.Lax =
    -- Install dependencies directly into the clean environment
    installDependencies cleanOS (cleanSource target) buildDepends sourceDependencies >>=
    failing (return . Failure) (prepareTree noClean)
    where
      prepareTree True _ =
          q12 "Finding build tree" $
          findOneDebianBuildTree newPath >>=
          return . failing (\ msgs -> Failure (("No build tree at " ++ show newPath) : msgs)) Success
      prepareTree False _ =
          q12 "Copying build tree..." $
          Debian.Repo.syncEnv cleanOS buildOS >>=
          const (try (copyDebianBuildTree (cleanSource target) newPath)) >>=
          return . either (\ (e :: SomeException) -> Failure [show e]) Success
      buildDepends = (P.buildDepends (P.params cache))
      noClean = P.noClean (P.params cache)
      newPath = rootPath (rootDir buildOS) ++ fromJust (dropPrefix (rootPath (rootDir cleanOS)) oldPath)
      oldPath = topdir . cleanSource $ target
prepareBuildImage cache cleanOS sourceDependencies buildOS target _ =
    -- Install dependencies directly into the build environment
    findTree noClean >>=
    failing (return . Failure) downloadDeps >>=
    failing (return . Failure) (syncEnv noClean) >>=
    failing (return . Failure) installDeps
    where
      -- findTree :: Bool -> IO (Failing DebianBuildTree)
      findTree False =
          q12 "Finding build tree" $
              try (copyDebianBuildTree (cleanSource target) newPath) >>=
              return . either (\ (e :: SomeException) -> Failure [show e]) Success
      findTree True =
          q12 "Finding build tree" $
              findOneDebianBuildTree newPath

      downloadDeps buildTree = downloadDependencies cleanOS buildTree buildDepends sourceDependencies >>=
                               failing (return . Failure) (const (return (Success buildTree)))

      syncEnv False buildTree =
          q12 "Syncing buildOS" $
              Debian.Repo.syncEnv cleanOS buildOS >>= (\ os -> return (Success (os, buildTree)))
      syncEnv True buildTree =
          return (Success (buildOS, buildTree))

      installDeps (buildOS, buildTree) = installDependencies buildOS buildTree buildDepends sourceDependencies >>=
                                         failing (return . Failure) (const (return (Success buildTree)))
      buildDepends = P.buildDepends (P.params cache)
      noClean = P.noClean (P.params cache)
      newPath = rootPath (rootDir buildOS) ++ fromJust (dropPrefix (rootPath (rootDir cleanOS)) (topdir (cleanSource target)))
            
-- | Get the control info for the newest version of a source package
-- available in a release.  Make sure that the files for this build
-- architecture are available.
getReleaseControlInfo :: OSImage -> String -> (Maybe SourcePackage, SourcePackageStatus, String)
getReleaseControlInfo cleanOS packageName =
    case zip sourcePackages (map (isComplete binaryPackages) sourcePackagesWithBinaryNames) of
      (info, status@Complete) : _ -> (Just info, All, message status)
      (info, status@(Missing missing)) : _ -> (Just info, Indep missing, message status)
      _ -> (Nothing, None, message Complete)
    where
      message status =
          intercalate "\n"
                  (["  Source Package Versions: " ++ show (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages),
                    "  Required Binary Package Names:"] ++
                   map (("   " ++) . show) (zip (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages) (map sourcePackageBinaryNames sourcePackages)) ++
                   missingMessage status ++
                   ["  Binary Package Versions: " ++ show (map (second prettyDebianVersion . binaryPackageVersion) binaryPackages),
                    "  Available Binary Packages of Source Package:"] ++
                   map (("   " ++) . show) (zip (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages) (map (availableDebNames binaryPackages) sourcePackages)))
      missingMessage Complete = []
      missingMessage (Missing missing) = ["  Missing Binary Package Names: "] ++ map ("   " ++) missing
      sourcePackagesWithBinaryNames = zip sourcePackages (map sourcePackageBinaryNames sourcePackages)
      binaryPackages = Debian.Repo.Cache.binaryPackages cleanOS (nub . concat . map sourcePackageBinaryNames $ sourcePackages)
      sourcePackages = sortBy compareVersion . Debian.Repo.Cache.sourcePackages cleanOS $ [packageName]
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
      isComplete :: [BinaryPackage] -> (SourcePackage, [String]) -> Status
      isComplete binaryPackages (sourcePackage, requiredBinaryNames) =
          if missingDebs == Set.empty && (unableToCheckUDebs || missingUdebs == Set.empty)
          then Complete
          else Missing (Set.toList missingDebs ++ Set.toList missingUdebs)
          where
            (_readyDebs, missingDebs) = Set.partition (`Set.member` availableDebs) required
            (_readyUdebs, missingUdebs) =
                if unableToCheckUDebs
                then (Set.empty, Set.empty)
                else Set.partition (`Set.member` (Set.union availableDebs availableUDebs)) required
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

data Status = Complete | Missing [String]

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
computeNewVersion :: P.CacheRec -> [SourcePackage] -> Maybe SourcePackage -> DebianVersion -> Failing DebianVersion
computeNewVersion cache
                  available		-- All the versions that exist in the pool in any dist,
					-- the new version number must not equal any of these.
                  current		-- The control file paragraph for the currently uploaded
                                        -- version in this dist.  The new version must be newer
                                        -- than this.
                  sourceVersion =	-- Version number in the changelog entry of the checked-out
                                        -- source code.  The new version must also be newer than this.
    case P.doNotChangeVersion (P.params cache) of
      True -> Success sourceVersion
      False ->
          let vendor = P.vendorTag (P.params cache)
              oldVendors = P.oldVendorTags (P.params cache)
              release = if (P.isDevelopmentRelease (P.params cache)) then
                            Nothing else
                            (Just (sliceName (P.baseRelease (P.params cache))))
              extra = P.extraReleaseTag (P.params cache) 
              aliases = \ x -> maybe x id (lookup x (P.releaseAliases (P.params cache))) in
          case parseTag (vendor : oldVendors) sourceVersion of

            (_, Just tag) -> Failure ["Error: the version string in the changelog has a vendor tag (" ++ show tag ++
                                      ".)  This is prohibited because the autobuilder needs to fully control suffixes" ++
                                      " of this form.  This makes it difficult for the author to know what version" ++
                                      " needs to go into debian/changelog to trigger a build by the autobuilder," ++
                                      " particularly since each distribution may have different auto-generated versions."]
            (_, Nothing) -> setTag aliases vendor oldVendors release extra currentVersion (catMaybes . map getVersion $ available) sourceVersion >>= checkVersion
    where
      getVersion paragraph =
          maybe Nothing (Just . parseDebianVersion . B.unpack) (fieldValue "Version" . sourceParagraph $ paragraph)
      currentVersion =
          maybe Nothing (Just . parseDebianVersion . B.unpack) (maybe Nothing (fieldValue "Version" . sourceParagraph) current)
      checkVersion :: DebianVersion -> Failing DebianVersion
      checkVersion result =
          maybe (Success result)
                (\ v -> if result <= v
                        then Failure ["Autobuilder bug: new version number " ++ show (prettyDebianVersion result) ++ " is not newer than current version number " ++ show (prettyDebianVersion v)]
                        else Success result)
                currentVersion

-- FIXME: Most of this code should move into Debian.Repo.Dependencies
buildDepSolutions' :: [String] -> OSImage -> Relations -> Control -> IO (Failing [(Int, [BinaryPackage])])
buildDepSolutions' preferred os globalBuildDeps debianControl =
    q12 "Searching for build dependency solution" $
    do
      arch <- buildArchOfEnv (rootDir os)
      -- We don't discard any dependencies here even if they are
      -- mentioned in Relax-Depends, that only applies to deciding
      -- whether to build, once we are building we need to install all
      -- the dependencies.  Hence this empty list.
      case G.buildDependencies debianControl of
        Left s -> return (Failure [s])
        Right (_, relations, _) ->
            do let relations' = relations ++ globalBuildDeps
                   relations'' = simplifyRelations packages relations' preferred arch
               -- Do not stare directly into the solutions!  Your head will
               -- explode (because there may be a lot of them.)  Also, this
               -- will be slow if solutions is not compiled.
               case Debian.Repo.Dependencies.solutions packages (filter (not . alwaysSatisfied) relations'') 100000 of
                 Left error -> return (Failure [error, message relations' relations''])
                 Right solutions -> quieter (+ 1) $ qPutStrLn (message relations' relations'') >> return (Success solutions)
    where
      alwaysSatisfied xs = any isNothing xs && all isNothing xs
      packages = aptBinaryPackages os
      message relations' relations'' =
          "Build dependency relations:\n " ++
          concat (intersperse "\n " (map (\ (a, b) -> show (map prettyRelation a) ++ " -> " ++ show (map prettySimpleRelation b))
                                              (zip relations' relations'')))
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

-- In ghc610, using readFile on pseudo files in /proc hangs.  Use this instead.
--rf path = lazyCommand ("cat '" ++ path ++ "'") L.empty >>= return . (\ (o, _, _) -> o) . collectOutputUnpacked

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
updateChangesFile :: NominalDiffTime -> ChangesFile -> IO ChangesFile
updateChangesFile elapsed changes =
    q12 "Updating changes file" $
    do
      let (Paragraph fields) = changeInfo changes
{-    autobuilderVersion <- processOutput "dpkg -s autobuilder | sed -n 's/^Version: //p'" >>=
                            return . either (const Nothing) Just >>=
                            return . maybe Nothing (listToMaybe . lines) -}
      hostname <- lazyCommandF "hostname" L.empty >>= return . listToMaybe . lines . L.unpack . stdoutOnly
      cpuInfo <- parseProcCpuinfo
      memInfo <- parseProcMeminfo
      machine <- lazyCommandF "uname -m" L.empty >>= return . listToMaybe . lines . L.unpack . stdoutOnly
      let buildInfo = ["Autobuilder-Version: " ++ V.autoBuilderVersion] ++
                      ["Time: " ++ show elapsed] ++
                      maybeField "Memory: " (lookup "MemTotal" memInfo) ++
                      maybeField "CPU: " (lookup "model name" cpuInfo) ++
                      ["CPU count: " ++ (show . length . lookupAll "processor" $ cpuInfo)] ++
                      maybeField "OS Architecture: " machine ++
                      maybeField "CPU MHz: " (lookup "cpu MHz" cpuInfo) ++
                      maybeField "CPU cache: " (lookup "cache size" cpuInfo) ++
                      maybeField "Host: " hostname
      let fields' = sinkFields (== "Files")
                    (Paragraph $ fields ++ [Field ("Build-Info", "\n " ++ intercalate "\n " buildInfo)])
      -- let changes' = changes {changeInfo = Paragraph fields'}
      -- replaceFile (Debian.Repo.path changes') (show (Control [fields']))
      return changes {changeInfo = fields'}
    where
      maybeField tag value = maybe [] ((: []) . (tag ++)) value

-- |Move this to {-Debian.-} Control
sinkFields :: (Eq a) => (a -> Bool) -> Paragraph' a -> Paragraph' a
sinkFields f (Paragraph fields) =
    let (a, b) = partition f' fields in Paragraph (b ++ a)
    where f' (Field (name, _)) = f name
          f' (Comment _) = False

-- |Download the package's build dependencies into /var/cache
downloadDependencies :: OSImage -> DebianBuildTree -> [String] -> [PkgVersion] -> IO (Failing String)
downloadDependencies os source extra versions =
    
    do -- qPutStrLn "Downloading build dependencies"
       vers <- liftIO (evaluate versions)
       quieter (+ 1) $ qPutStrLn . intercalate "\n  " $ "Dependency package versions: " : map (show . prettyPkgVersion) vers
       qPutStrLn ("Downloading build dependencies into " ++ rootPath (rootDir os))
       (out, _, code) <- useEnv' (rootPath root) forceList (lazyCommandE command L.empty) >>=
                         return . collectOutputUnpacked . mergeToStdout
       case code of
         ExitSuccess -> return (Success out)
         code -> qMessage ("FAILURE: " ++ command ++ " -> " ++ show code ++ "\n" ++ out) () >>
                 return (Failure ["FAILURE: " ++ command ++ " -> " ++ show code ++ "\nOutput:\n" ++ out])
    where
      command = ("export DEBIAN_FRONTEND=noninteractive; " ++
                 (if True then aptGetCommand else pbuilderCommand))
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes --force-yes install -o APT::Install-Recommends=True --download-only " ++ intercalate " " (map showPkgVersion versions ++ extra)
      path = pathBelow (rootPath root) (topdir source)
      root = rootDir os

pathBelow :: FilePath -> FilePath -> FilePath
pathBelow root path =
    maybe (error message) id (dropPrefix root path)
    where message = "Expected a path below " ++ root ++ ", saw " ++ path

-- |Install the package's build dependencies.
installDependencies :: OSImage -> DebianBuildTree -> [String] -> [PkgVersion] -> IO (Failing [Output])
installDependencies os source extra versions =
    do qPutStrLn $ "Installing build dependencies into " ++ rootPath (rootDir os)
       (code, out) <- Proc.withProc os (useEnv' (rootPath root) forceList $ lazyCommandV command L.empty) >>= return . collectResult
       case code of
         ExitSuccess -> return (Success out)
         code -> quieter (const 0) $ qPutStrLn ("FAILURE: " ++ command ++ " -> " ++ show code ++ "\n" ++ outputToString out) >>
                 return (Failure ["FAILURE: " ++ command ++ " -> " ++ show code])
    where
      command = ("export DEBIAN_FRONTEND=noninteractive; " ++
                 (if True then aptGetCommand else pbuilderCommand))
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes --force-yes install -o APT::Install-Recommends=True " ++ intercalate " " (map showPkgVersion versions ++ extra)
      --aptGetCommand = "apt-get --yes build-dep -o APT::Install-Recommends=False " ++ sourcpackagename
      path = pathBelow (rootPath root) (topdir source)
      root = rootDir os

-- | This should probably be what the real useEnv does.
useEnv' :: FilePath -> (a -> IO a) -> IO a -> IO a
useEnv' rootPath force action = quieter (+ 1) $ useEnv rootPath force $ quieter (+ (-1)) action

-- | Move to System.Unix.Process?
outputToString :: [Output] -> String
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
setRevisionInfo :: Exception e => DebianVersion -> Either e String -> [PkgVersion] -> ChangesFile -> IO ChangesFile
setRevisionInfo sourceVersion revision versions changes {- @(Changes dir name version arch fields files) -} =
    q12 "Setting revision info" $
    case partition isDscFile (changeFiles changes) of
      ([file], otherFiles) ->
          do
            let dscFilePath = changeDir changes ++ "/" ++ changedFileName file
            newDscFile <- parseControlFromFile dscFilePath >>= return . either (error . show) addField
            replaceFile dscFilePath (show newDscFile)
            md5 <- md5sum dscFilePath
            sha1 <- sha1sum dscFilePath
            sha256 <- sha256sum dscFilePath
            case (md5, sha1, sha256) of
              (Success md5, Success sha1, Success sha256) ->
                  do
                    size <- getFileStatus dscFilePath >>= return . fileSize
                    let changes' = changes {changeFiles = (otherFiles ++ [file {changedFileMD5sum = md5, changedFileSHA1sum = sha1, changedFileSHA256sum = sha256, changedFileSize = size}])}
                    Debian.Repo.Changes.save changes'
                    return changes'
              e -> error (show e)
      -- A binary only build will have no .dsc file
      ([], _) -> return changes
      (several, _) -> error ("Multiple .dsc files found in source package: " ++ intercalate ", " (map (show . prettyChanges) several))
    where
      addField (Control (Paragraph sourceInfo : binaryInfo)) =
          Control (newSourceInfo : binaryInfo)
          where newSourceInfo = raiseFields (/= "Files") (Paragraph (sourceInfo ++ [newField]))
      addField (Control []) = error "Invalid control file"
      newField = Field ("Revision", " " ++ newFieldValue)
      newFieldValue = either (error . show) id revision ++ " " ++ show (prettyDebianVersion sourceVersion) ++ " " ++ formatVersions versions
      formatVersions versions = intercalate " " (map showPkgVersion versions)
      isDscFile file = isSuffixOf ".dsc" $ changedFileName file

-- | Run a checksum command on a file, return the resulting checksum as text.
doChecksum :: String -> (String -> String) -> FilePath -> IO (Failing String)
doChecksum cmd f path =
    lazyProcess cmd' [path] Nothing Nothing L.empty >>=
    return . either (doError (cmd' ++ " " ++ path)) (Success . f) . toEither . collectOutputUnpacked
    where cmd' = "/usr/bin/" ++ cmd

md5sum :: FilePath -> IO (Failing String)
md5sum = doChecksum "md5sum" (take 32)
sha1sum :: FilePath -> IO (Failing String)
sha1sum = doChecksum "sha1sum" (take 40)
sha256sum :: FilePath -> IO (Failing String)
sha256sum = doChecksum "sha256sum" (take 64)

toEither :: (a, String, ExitCode) -> Either (a, String, ExitCode) a
toEither (text, "", ExitSuccess) = Right text
toEither x = Left x

doError :: Show a => String -> (t, a, ExitCode) -> Failing a
doError cmd (_, s, ExitSuccess) = Failure ["Unexpected error output from " ++ cmd ++ ": " ++ show s]
doError cmd (_, _, ExitFailure n) = Failure ["Error " ++ show n ++ " running '" ++ cmd ++ "'"]

-- |Decide whether to build a package.  We will build if the revision
-- is different from the revision of the uploaded source, or if any of
-- the build dependencies are newer than the versions which were
-- encoded into the uploaded version's control file.
buildDecision :: P.CacheRec
              -> Target
              -> Maybe DebianVersion	-- builtVersion: the version already present in the repository
              -> Maybe DebianVersion	-- builtSrcVersion: the version of the source code that builtVersion was built from
              -> Maybe String		-- builtRevision: that version's revision string
              -> [PkgVersion]		-- builtDependencies: the list of of dependencies for that version
              -> SourcePackageStatus	-- releaseStatus: the status of the version in the repository
              -> DebianVersion		-- sourceVersion: the version number in the newest changelog entry of the source code
              -> [PkgVersion]		-- sourceDependencies: the list of build dependency versions computed from the build environment
              -> BuildDecision
buildDecision cache target 
                  oldVersion oldSrcVersion _oldRevision builtDependencies releaseStatus
                  sourceVersion sourceDependencies =
    case oldVersion == Nothing of
      _ | forceBuild -> Yes "--force-build option is set"
        | isNothing oldVersion -> Yes ("Initial build of version " ++ show (prettyDebianVersion sourceVersion))
      _ ->
          case isJust oldSrcVersion of
            True ->
                case compare sourceVersion (fromJust oldSrcVersion) of
                  GT -> Yes ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is newer than released source version (" ++ show (prettyDebianVersion (fromJust oldSrcVersion)) ++ ")")
                  LT -> No ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is trumped by released source version (" ++ show (prettyDebianVersion (fromJust oldSrcVersion)) ++ ")")
                  EQ -> sameSourceTests
            False ->
                case compare (dropTag allTags sourceVersion) (dropTag allTags (fromJust oldVersion)) of
                  GT -> Yes ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is newer than released source version (" ++ show (prettyDebianVersion (fromJust oldVersion)) ++ ")")
                  LT -> No ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is trumped by released source version (" ++ show (prettyDebianVersion (fromJust oldVersion)) ++ ")")
                  EQ ->
                      case dropTag allTags sourceVersion == sourceVersion of
                        False -> Yes ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is tagged, and old source version was not recorded")
                        True -> sameSourceTests
    where
      vendorTag = P.vendorTag (P.params cache)
      oldVendorTags = P.oldVendorTags (P.params cache)
      forceBuild = elem (targetName target) (P.forceBuild (P.params cache))
      -- discardTarget = Set.member (targetName target) (P.discard (P.params cache))
      allowBuildDependencyRegressions = P.allowBuildDependencyRegressions (P.params cache)
      -- Build decision tests for when the version number of the
      -- source hasn't changed.  Note that the source itself may have
      -- changed, but we don't ask the SCCS whether that has happened.
      -- This is a design decision which avoids building from source
      -- that might have been checked in but isn't ready to be
      -- uploaded to the repository.  Note that if the build
      -- dependencies change the package will be built anyway, so we
      -- are not completely protected from this possibility.
      sameSourceTests =
          case releaseStatus of
            Indep missing | missing /= [] && not (notArchDep (targetControl target)) ->
                  -- The binary packages are missing, we need an arch only build.
                  Arch ("Version " ++ maybe "Nothing" show (fmap prettyDebianVersion oldVersion) ++ " needs arch only build. (Missing: " ++ show missing ++ ")")
            _ | badDependencies /= [] && not allowBuildDependencyRegressions ->
                  Error ("Build dependency regression (allow with --allow-build-dependency-regressions): " ++ 
                         concat (intersperse ", " (map (\ ver -> show (fmap prettyPkgVersion (builtVersion ver)) ++ " -> " ++ show (prettyPkgVersion ver)) badDependencies)))
              | badDependencies /= [] ->
                  Auto ("Build dependency regression: " ++ 
                        concat (intersperse ", " (map (\ ver -> show (fmap prettyPkgVersion (builtVersion ver)) ++ " -> " ++ show (prettyPkgVersion ver)) badDependencies)))
              | autobuiltDependencies /= [] && isNothing oldSrcVersion ->
		  -- If oldSrcVersion is Nothing, the autobuilder didn't make the previous build
                  -- so there are no recorded build dependencies.  In that case we don't really
                  -- know whether a build is required, so we could go either way.  The decision
                  -- here is to only built if some of the build dependencies were built by the
                  -- autobuilder (so their version numbers have been tagged by it.)
                  Auto ("Build dependency status unknown:\n" ++ buildDependencyChangeText autobuiltDependencies)
              | (revvedDependencies ++ newDependencies) /= [] && isJust oldSrcVersion ->
                  -- If the package *was* previously built by the autobuilder we rebuild when any
                  -- of its build dependencies are revved or new ones appear.
                  Auto ("Build dependencies changed:\n" ++ buildDependencyChangeText (revvedDependencies ++ newDependencies))
            Indep _ | notArchDep (targetControl target) ->
                  No ("Version " ++ show (prettyDebianVersion sourceVersion) ++ " of architecture independent package is already in release.")
            Indep missing ->
                  -- The binary packages are missing, we need an arch only build.
                  Arch ("Version " ++ maybe "Nothing" show (fmap prettyDebianVersion oldVersion) ++ " needs arch only build. (Missing: " ++ show missing ++ ")")
            All ->
                  No ("Version " ++ show (prettyDebianVersion sourceVersion) ++ " is already in release.")
            _ ->
                  error ("Unexpected releaseStatus: " ++ show releaseStatus)
      notArchDep control =
          all (== "all") . map (maybe "all" strip) . map (lookupP "Architecture") . unControl $ control
          where strip (Field (_, s)) = stripWS s
                strip (Comment _) = ""
      buildDependencyChangeText dependencies =
          "  " ++ intercalate "\n  " lines
          where
            lines = map (\ (built, new) -> show (fmap prettyPkgVersion built) ++ " -> " ++ show (prettyPkgVersion new)) (zip builtVersions dependencies)
            builtVersions = map (findDepByName builtDependencies) dependencies
            findDepByName builtDependencies new = find (\ old -> getName new == getName old) builtDependencies
      -- The list of the revved and new dependencies which were built by the autobuilder.
      autobuiltDependencies = filter isTagged (revvedDependencies ++ newDependencies)
      isTagged :: PkgVersion -> Bool
      isTagged dep = isJust . snd . parseTag allTags . getVersion $ dep
      allTags :: [String]
      allTags = vendorTag : oldVendorTags
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
            -- If any dependency is older than the one we last built with it is an error.
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
      -- Remove any package not mentioned in the relaxed dependency list
      -- from the list of build dependencies which can trigger a rebuild.
      sourceDependencies' = filter (\ x -> elem (getName x) (packageNames (targetRelaxed (P.relaxDepends (P.params cache)) target))) sourceDependencies
      -- All the package names mentioned in a dependency list
      packageNames :: G.DepInfo -> [String]
      packageNames (_, deps, _) = nub (map (\ (Rel name _ _) -> name) (concat deps))
