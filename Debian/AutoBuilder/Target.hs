{-# LANGUAGE ScopedTypeVariables #-}
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
    , partitionFailing, ffe, eff
    ) where

import Control.Applicative.Error (Failing(..), failing)
import Control.Arrow(second)
import Control.Exception(SomeException, try, evaluate)
import Control.Monad.RWS(MonadIO(..), MonadTrans(..), when)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List(intersperse, find, intercalate, intersect, isSuffixOf,
                 nub, partition, sortBy)
import qualified Data.Map as Map
import Data.Maybe(catMaybes, fromJust, isJust, isNothing, listToMaybe)
import qualified Data.Set as Set
import Data.Time(NominalDiffTime)
import Debian.AutoBuilder.BuildTarget(BuildTarget(..), Tgt(..), prepareDir)
import qualified Debian.AutoBuilder.BuildTarget as BuildTarget
import qualified Debian.AutoBuilder.BuildTarget.Apt as Apt
import qualified Debian.AutoBuilder.BuildTarget.Cd as Cd
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
import qualified Debian.AutoBuilder.ParamClass as P
import qualified Debian.AutoBuilder.Version as V
import Debian.Changes (prettyChanges, ChangesFile(changeRelease, changeInfo, changeFiles, changeDir),
                       ChangedFileSpec(changedFileSize, changedFileName, changedFileMD5sum, changedFileSHA1sum, changedFileSHA256sum),
                       ChangeLogEntry(logWho, logVersion, logPackage, logDists, logDate, logComments))
import Debian.Control
import qualified Debian.Control.String as S(fieldValue)
import Debian.Extra.CIO(vMessage, vEPutStrBl, setStyle, vBOL, vEPutStr)
import qualified Debian.GenBuildDeps as G
import Debian.Relation.ByteString(Relations, Relation(..))
import Debian.Release (releaseName')
import Debian.Sources (SliceName(..))
import Debian.Repo (countTasks, save, uploadLocal, invalidRevision, readPkgVersion, showPkgVersion,
                    simplifyRelations, solutions, binaryPackages, buildArchOfEnv, sourcePackages,
                    aptSourcePackagesSorted, binaryPackageSourceVersion, sourcePackageBinaryNames,
                    scanIncoming, showErrors, OSImage, chrootEnv, syncEnv, syncPool, updateEnv,
                    findDebianSourceTree, SourceTreeC(..), DebianSourceTreeC(..), DebianSourceTree,
                    DebianBuildTree, DebianBuildTreeC(..), addLogEntry, copyDebianBuildTree,
                    copyDebianSourceTree, explainSourcePackageStatus, findChanges,
                    findDebianBuildTree, findOneDebianBuildTree, SourcePackageStatus(..))
import Debian.Repo.Monad (AptIOT)
import Debian.Repo.Types (SourcePackage(sourceParagraph, sourcePackageID),
                          AptCache(rootDir, aptBinaryPackages), EnvRoot(rootPath),
                          PackageID(packageVersion, packageName), LocalRepository, PkgVersion(..),
                          BinaryPackage(packageInfo, packageID))
import Debian.Shell(dotOutput)
import Debian.Sources (SliceName(..))
import Debian.Time(getCurrentLocalRFC822Time)
import Debian.Version(DebianVersion, parseDebianVersion, version)
import Debian.VersionPolicy(dropTag, parseTag, setTag)
import System.IO (hPutStrLn, stderr)
import System.Unix.Process(Output(..), collectOutputUnpacked,
                           commandOutput, lazyCommand, lazyProcess)
import Extra.Either(partitionEithers)
import Extra.Files(replaceFile)
import Extra.List(dropPrefix)
import Extra.Misc(columns, processOutput)
import System.Chroot (useEnv, forceList')
import System.Directory(renameDirectory)
import System.Exit(ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files(fileSize, getFileStatus)
import Text.Printf(printf)
import Text.Regex(matchRegex, mkRegex)

--liftTIO = lift

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
            , Cd.documentation
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
             , targetDepends :: G.DepInfo	-- ^ The dependency info parsed from the control file
             , targetRelaxed :: G.DepInfo	-- ^ The dependency info with some dependencies relaxed.
						--   Note that the entire list of targets is required to
						--   compute this so we can look at the dependency graph.
             }

instance Eq Target where
    a == b = targetName a == targetName b

{-
instance Show Target where
    show target = show . realSource $ target
-}

-- | The /Source:/ attribute of debian\/control.
targetName :: Target -> String
targetName target =
    case removeCommentParagraphs (targetControl target) of
      (Control (paragraph : _)) ->
          maybe (error "Missing Source field") id $ fieldValue "Source" paragraph
      _ -> error "Target control information missing"

findSourceParagraph (Control paragraphs) = 
    case dropWhile isCommentParagraph paragraphs of
      (paragraph : _) -> Just paragraph
      _ -> Nothing
    where
      isCommentParagraph (Paragraph fields) = all isCommentField fields
      isCommentField (Comment _) = True
      isCommentField _ = False

removeCommentParagraphs (Control paragraphs) =
    Control (filter (not . isCommentParagraph) paragraphs)
    where
      isCommentParagraph (Paragraph fields) = all isCommentField fields
      isCommentField (Comment _) = True
      isCommentField _ = False

countAndPrepareTargets :: (P.RunClass p) => p -> OSImage -> [Tgt] -> IO [Target]
countAndPrepareTargets params os tgts =
    let tasks = map (prepareTarget params os) tgts in
    countTasks' (zip (map show tgts) tasks) >>= \ targets ->
    case partitionFailing targets of
      ([], targets') -> return targets'
      (failures, _) -> error ("Could not prepare some targets:\n  " ++ intercalate "\n  " (map (intercalate ", ") failures))

-- | Perform a list of tasks with log messages.
countTasks' :: [(String, IO Target)] -> IO [Failing Target]
countTasks' tasks =
    mapM (countTask (length tasks)) (zip [1..] tasks)
    where
      countTask :: Int -> (Int, (String, IO a)) -> IO (Failing a)
      countTask count (index, (message, task)) =
          (hPutStrLn stderr (printf "[%2d of %2d] %s:" index count message)) >>
          try task >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success

-- |Move to Control.Applicative.Error.
partitionFailing :: [Failing a] -> ([[String]], [a])
partitionFailing =
    foldr f ([], [])
    where f (Success x) (fs, ss) = (fs, x : ss)
          f (Failure x) (fs, ss) = (x : fs, ss)

-- |Prepare a target for building in the given environment.  At this
-- point, the target needs to be a DebianSourceTree or a
-- DebianBuildTree. 
prepareTarget :: (P.RunClass p) => p -> OSImage -> Tgt -> IO Target
prepareTarget params os tgt@(Tgt source) =
    prepareBuild params os source >>= target
    where
      target :: DebianBuildTree -> IO Target
      target tree =
          try (BuildTarget.revision params source) >>= \ rev ->
          return $ Target { realSource = tgt
                          , cleanSource = tree
                          , targetEntry = latest
                          , targetControl = ctl
                          , targetVersion = ver
                          , targetRevision = either (\ (_ :: SomeException) -> Nothing) Just rev
                          , targetDepends = undefined
                          , targetRelaxed = undefined
                          }
          where
            ctl = control tree
            latest = entry tree
            ver = logVersion latest

{-          
    do tree <- prepareBuild params os source
               -- return . failing (\ msgs -> Failure ("Could not find Debian build tree for " ++ show source : msgs)) id
       let ctl = control tree
           latest = entry tree
           ver = logVersion latest
       rev <- try (BuildTarget.revision params source)
       return $ Target { realSource = tgt
                       , cleanSource = tree
                       , targetEntry = latest
                       , targetControl = ctl
                       , targetVersion = ver
                       -- Why would rev fail?
                       , targetRevision = either (\ (_ :: SomeException) -> Nothing) Just rev
                       , targetDepends = undefined
                       , targetRelaxed = undefined
                       }
-}

-- Failing From Either - use during conversion
ffe :: IO a -> IO (Failing a)
ffe a = try a >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success

eff (Failure ss) = Left (intercalate "\n" ss)
eff (Success x) = Right x

-- |'prepareBuild' returns a Debian build tree for a target with all
-- the revision control files associated with the old target removed.
-- This ensures that the tarball and\/or the .diff.gz file in the deb
-- don't contain extra junk.  It also makes sure that debian\/rules is
-- executable.
prepareBuild :: (P.RunClass p, BuildTarget t) => p -> OSImage -> t -> IO DebianBuildTree
prepareBuild params os target =
    do debBuild <- findOneDebianBuildTree (getTop params target)
       case debBuild of
         Success tree -> copyBuild tree
         Failure msgs ->
             hPutStrLn stderr ("Build tree not found, creating new one\n  " ++ intercalate "\n  " msgs) >>
             findDebianSourceTree (getTop params target) >>=
             copySource
    where
      copySource :: DebianSourceTree -> IO DebianBuildTree
      copySource debSource =
          do let name = logPackage . entry $ debSource
                 dest = rootPath (rootDir os) ++ "/work/build/" ++ name
                 ver = Debian.Version.version . logVersion . entry $ debSource
                 newdir = escapeForBuild $ name ++ "-" ++ ver
             --io $ System.IO.hPutStrLn System.IO.stderr $ "copySource " ++ show debSource
             copy <- copyDebianSourceTree debSource (dest ++ "/" ++ newdir)
             -- Clean the revision control files for this target out of the copy of the source tree
             cleanTarget params target (topdir copy)
             findDebianBuildTree dest newdir
      copyBuild :: DebianBuildTree -> IO DebianBuildTree
      copyBuild debBuild =
          do let name = logPackage . entry $ debBuild
                 dest = rootPath (rootDir os) ++ "/work/build/" ++ name
                 ver = Debian.Version.version . logVersion . entry $ debBuild
                 newdir = escapeForBuild $ name ++ "-" ++ ver
             --io $ System.IO.hPutStrLn System.IO.stderr $ "copyBuild " ++ show debBuild
             copy <- copyDebianBuildTree debBuild dest
             cleanTarget params target (topdir copy)
             when (newdir /= (subdir debBuild))
                      (liftIO $ renameDirectory (dest ++ "/" ++ subdir debBuild) (dest ++ "/" ++ newdir))
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
      depChanges _ = "  * Build dependency changes:" ++ prefix ++ intercalate prefix padded ++ "\n"
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
    intercalate prefix (map show buildDeps) ++
    "\n"
    where prefix = "\n    "

--  (P.debug params) (P.topDir params) (P.flushSource params) (P.ifSourcesChanged params) (P.allSources params)

readSpec :: (P.RunClass p) => p -> String -> AptIOT IO Tgt
readSpec params text =
    lift (vEPutStrBl 0 (text ++ ":")) >>
    {-setStyle (appPrefix " ")-}
    (case text of
            'a':'p':'t':':' : target -> Apt.prepareApt params target
            'd':'a':'r':'c':'s':':' : target -> lift (Darcs.prepareDarcs params target)
            'd':'e':'b':'-':'d':'i':'r':':' : target ->
                parsePair target >>= \ (upstream, debian) -> liftIO (DebDir.prepareDebDir params upstream debian)
            'c':'d':':' : dirAndTarget ->
                do let (subdir, target) = second tail (break (== ':') dirAndTarget)
                   readSpec params target >>= liftIO . Cd.prepareCd params subdir
            'd':'i':'r':':' : target -> lift $ prepareDir params target
            'h':'g':':' : target -> lift $ Hg.prepareHg params target
            'q':'u':'i':'l':'t':':' : target ->
                parsePair target >>= \ (base, patch) -> lift (Quilt.prepareQuilt params base patch)
            's':'o':'u':'r':'c':'e':'d':'e':'b':':' : target ->
                readSpec params target >>= liftIO . SourceDeb.prepareSourceDeb params
            's':'v':'n':':' : target -> lift $ Svn.prepareSvn params target
            't':'l':'a':':' : target -> lift $ Tla.prepareTla params target
            'b':'z':'r':':' : target -> lift $ Bzr.prepareBzr params target
            'u':'r':'i':':' : target -> lift $ Uri.prepareUri params target
            'p':'r':'o':'c':':' : target ->
                readSpec params target >>= lift . Proc.prepareProc params
            _ -> fail ("Error in target specification: " ++ text))
    where
      parsePair :: String -> AptIOT IO (Tgt, Tgt)
      parsePair text =
          case match "\\(([^)]*)\\):\\(([^)]*)\\)" text of
            Just [baseName, patchName] ->
                do a <- readSpec params baseName
                   b <- readSpec params patchName
                   return (a, b)
            _ -> error ("Invalid spec name: " ++ text)
      match = matchRegex . mkRegex
      addTargetName = failing (\ msgs -> Failure (("Target " ++ text ++ "failed") : msgs)) Success

-- | Build a set of targets.  When a target build is successful it
-- is uploaded to the incoming directory of the local repository,
-- and then the function to process the incoming queue is called.
buildTargets :: (AptCache t, P.RunClass p) => p -> OSImage -> Relations -> LocalRepository -> t -> [Tgt] -> AptIOT IO (LocalRepository, [Target])
buildTargets _ _ _ localRepo _ [] = return (localRepo, [])
buildTargets params cleanOS globalBuildDeps localRepo poolOS targetSpecs =
    do
      -- showTargets targetSpecs
      targetList <- lift $ prepareAllTargetSource cleanOS
      lift (vEPutStrBl 0 "Building all targets:")
      failed <- {- setStyle (addPrefix " ") $ -} buildLoop cleanOS globalBuildDeps (length targetList) (targetList, [])
      return (localRepo, failed)
      --buildAll cleanOS targetList globalBuildDeps
    where
      -- Retrieve and/or update the source code of all the targets before building.
      prepareAllTargetSource cleanOS =
          do
            vEPutStrBl 0 "Assembling clean source tree for each target:"
            ({- debugStyle . -} iStyle) $ countAndPrepareTargets params cleanOS targetSpecs
          where
            iStyle = setStyle (appPrefix " ")
      -- Execute the target build loop until all the goals (or everything) is built
      buildLoop :: OSImage -> Relations -> Int -> ([Target], [Target]) -> AptIOT IO [Target]
      buildLoop _ _ _ ([], failed) = return failed
      buildLoop cleanOS globalBuildDeps count (unbuilt, failed) =
          do
            relaxed <- lift $ updateDependencyInfo globalBuildDeps (P.relaxDepends params) unbuilt
            next <- lift $ chooseNextTarget (goals relaxed) relaxed
            case next of
              Nothing -> return failed
              Just (target, blocked, other) ->
                  do
                    --tio (vEPutStrBl 0 ("\n\n" ++ makeTable targetGroups ++ "\n"))
                    lift (vEPutStrBl 0 (printf "[%2d of %2d] TARGET: %s - %s\n"
                                        (count - length relaxed + 1) count (targetName target) (show (realSource target))))
                    -- mapRWST (local (appPrefix " ")) (buildTarget' target) >>=
                    result <- buildTarget' target
                    failing (\ errs ->
                                 do lift $ vEPutStrBl 0 ("Package build failed:\n " ++ intercalate "\n " errs)
                                    lift $ vEPutStrBl 0 ("Discarding " ++ targetName target ++ " and its dependencies:\n  " ++
                                                         concat (intersperse "\n  " (map targetName blocked)))
                                    buildLoop cleanOS globalBuildDeps count (other, (target : blocked) ++ failed))
                            (\ _ ->
                                 do cleanOS' <- updateEnv cleanOS
                                    case cleanOS' of
                                      Left e -> error ("Failed to update clean OS:\n " ++ show e)
                                      Right cleanOS'' ->
                                          buildLoop cleanOS'' globalBuildDeps count (blocked ++ other, failed))
                            result
          where
            buildTarget' target =
                do lift (vBOL 0)
                   {- showElapsed "Total elapsed for target: " $ -} 
                   buildTarget params cleanOS globalBuildDeps localRepo poolOS target
      -- If no goals are given in the build parameters, assume all
      -- known targets are goals.
      goals targets =
          case P.goals params of
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
chooseNextTarget :: [Target] -> [Target] -> IO (Maybe (Target, [Target], [Target]))
chooseNextTarget [] _ = return Nothing
chooseNextTarget goals targets =
    -- Compute the list of build dependency groups, each of which
    -- starts with a target that is ready to build followed by
    -- targets which are blocked by the first target.
    case G.buildable depends targets of
      (G.CycleInfo arcs) ->
          error ("Dependency cycles formed by these edges need to be broken:\n  " ++
                 unlines (map (intercalate " ")
                          (columns (["these binary packages", "from this source package", "", "force a rebuild of"] :
                                    (map (\ (pkg, dep) -> [(show (intersect (binaryNames dep)
                                                                  (binaryNamesOfRelations (targetRelaxed pkg)))),
                                                           targetName dep, " -> ", targetName pkg]) arcs)))) ++
                 "\nAdd one or more of these lines (but as few as possible) to your configuration file:\n  " ++
                 intercalate "\n  " (map relaxLine (nub (concat (map pairs arcs)))) ++ "\n")
          where
            relaxLine (bin, src) = "Relax-Depends: " ++ bin ++ " " ++ src
            pairs (pkg, dep) =
                 map (\ bin -> (bin, targetName pkg)) binaryDependencies
                 where
                   binaryDependencies = intersect (binaryNames dep) (binaryNamesOfRelations (targetRelaxed pkg))
            binaryNamesOfRelations (_, rels, _) =
                concat (map (map (\ (Rel name _ _) -> name)) rels)
      info ->
          do vEPutStrBl 1 (makeTable info)
             return . listToMaybe . sortBy (compareReady goals) . G.readyTriples $ info
    where
      makeTable (G.BuildableInfo ready other) =
          unlines . map (intercalate " ") . columns $ goalsLine ++ readyLines
          where
            goalsLine = [{- [" Goals: ", "[" ++ intercalate ", " (map targetName goals) ++ "]"] -}]
            readyLines = map readyLine ready
            readyLine (ready, blocked, other) = 
                [" Ready:", targetName ready, "Blocking: [" ++ intercalate ", " (map targetName blocked) ++ "]"]
      -- We choose the next target using the relaxed dependency set
      depends :: Target -> Target -> Ordering
      depends target1 target2 = G.compareSource (targetRelaxed target1) (targetRelaxed target2)
      binaryNames dep =
          map (\ (G.BinPkgName name) -> name) xs
          where (_, _, xs) = (targetRelaxed dep)
      -- Choose the next target to build.  Look for targets which are
      -- in the goal list, or which block packages in the goal list.
      -- Among those, prefer the target which blocks the most
      -- packages.  If there are goal targets but none of them are
      -- ready to build or directly block 
      -- targets include a goal as readyamongoals none of the 
      compareReady :: [Target] -> (Target, [Target], [Target]) ->  (Target, [Target], [Target]) -> Ordering
      compareReady goals (aReady, aBlocked, _) (bReady, bBlocked, _) =
          -- Prefer targets which include a goal
          case compare (length bGoals) (length aGoals) of
            -- Otherwise, prefer the target which blocks the most other targets
            EQ -> compare (length bBlocked) (length aBlocked)
            x -> x
          where 
            aGoals = intersect goals (aReady : aBlocked)
            bGoals = intersect goals (bReady : bBlocked)
              
updateDependencyInfo :: Relations -> G.RelaxInfo -> [Target] -> IO [Target]
updateDependencyInfo globalBuildDeps relaxInfo targets =
    getDependencyInfo globalBuildDeps targets >>=
    (\ targets -> vEPutStrBl 1 ("Original dependencies: " ++ show (map targetDepends targets)) >> return targets) >>=
    return . relax relaxInfo >>=
    (\ targets -> vEPutStrBl 1 ("Relaxed dependencies:  " ++ show (map targetRelaxed targets)) >> return targets)

-- |Retrieve the dependency information for a list of targets
getDependencyInfo :: Relations -> [Target] -> IO [Target]
getDependencyInfo globalBuildDeps targets =
    mapM (getTargetDependencyInfo globalBuildDeps) targets >>=
    finish . partitionEithers . zipEithers targets . map eff
    where
      finish ([], ok) = return (map (\ (target, deps) -> target {targetDepends = deps}) ok)
      finish (bad, ok) =
          do -- FIXME: Any errors here should be fatal
             vEPutStrBl 0 ("Unable to retrieve build dependency info for some targets:\n  " ++
                           concat (intersperse "\n  " (map (\ (target, message) -> targetName target ++ ": " ++ message) bad)))
             return (map (\ (target, deps) -> target {targetDepends = deps}) ok)

zipEithers :: [a] -> [Either b c] -> [Either (a, b) (a, c)]
zipEithers xs ys = 
    map zipEither (zip xs ys)
    where
      zipEither :: (a, Either b c) -> Either (a, b) (a, c)
      zipEither (x, (Left y)) = Left (x, y)
      zipEither (x, (Right y)) = Right (x, y)

-- |Retrieve the dependency information for a single target
getTargetDependencyInfo :: Relations -> Target -> IO (Failing G.DepInfo)
getTargetDependencyInfo globalBuildDeps target =
    do let buildTree = cleanSource target
       --let sourceTree = debTree buildTree
       let controlPath = debdir buildTree ++ "/debian/control"
       info <- liftIO $ parseControlFromFile controlPath >>= return . either (Left . show) (G.buildDependencies . removeCommentParagraphs)
       -- vEPutStrBl 0 ("getDepInfo " ++ targetName target ++ ": " ++ show info)
       return $ either (Failure . (: [])) (\ deps -> Success (addRelations globalBuildDeps deps)) info
    where
      addRelations :: Relations -> G.DepInfo -> G.DepInfo
      addRelations moreRels (name, relations, bins) = (name, relations ++ moreRels, bins)

-- |Remove the relaxed dependencies from a list of targets.  We need
-- to do the entire list because G.relaxDeps needs all the DepInfo
-- objects to map source package names to binary package names and
-- vice versa.
relax :: G.RelaxInfo -> [Target] -> [Target]
relax relaxInfo targets =
    map (\ (target, d, r) -> target {targetDepends = d, targetRelaxed = r}) (zip3 targets deps deps')
    where
      deps' = G.relaxDeps relaxInfo deps
      deps = map targetDepends targets

showTargets :: [P.Target] -> String
showTargets targets =
    unlines (heading :
             map (const '-') heading :
             map concat (columns (map (\ (n, t) -> [printf "%4d. " n, P.sourcePackageName t, " ", P.sourceSpec t]) pairs))) ++ "\n"
    where
      pairs = zip [1..] targets :: [(Int, P.Target)]
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
    (AptCache t, P.RunClass p) =>
    p ->				-- configuration info
    OSImage ->				-- cleanOS
    Relations ->			-- The build-essential relations
    LocalRepository ->			-- The local repository the packages will be uploaded to
    t ->
    Target ->
    AptIOT IO (Failing LocalRepository)	-- The local repository after the upload, or an error message
buildTarget params cleanOS globalBuildDeps repo poolOS target =
    do
      lift (syncPool cleanOS)
      -- Get the control file from the clean source and compute the
      -- build dependencies
      let debianControl = targetControl target
      lift (vEPutStrBl 1 "Loading package lists and searching for build dependency solution...")
      solutions <- lift (iStyle $ buildDepSolutions' (P.preferred params) cleanOS globalBuildDeps debianControl)
      case solutions of
        Failure excuses -> do let excuses' = ("Couldn't satisfy build dependencies" : excuses)
                              lift (vEPutStrBl 0 (intercalate "\n " excuses'))
                              return $ Failure excuses
        Success [] -> error "Internal error 4"
        Success ((count, sourceDependencies) : _) ->
            do let sourceDependencies' = map makeVersion sourceDependencies
               lift (vEPutStrBl 2 ("Using build dependency solution #" ++ show count))
               lift (vEPutStr 3 (concat (map (("\n  " ++) . show) sourceDependencies')))
               -- Get the newest available version of a source package,
               -- along with its status, either Indep or All
               let (releaseControlInfo, releaseStatus, message) = getReleaseControlInfo cleanOS packageName
               lift (vEPutStrBl 2 message)
               lift (vEPutStrBl 2 ("Status of " ++ packageName ++ maybe "" (\ p -> "-" ++ show (packageVersion . sourcePackageID $ p)) releaseControlInfo ++
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
               sourceRevision <- case realSource target of (Tgt spec) -> lift (try (BuildTarget.revision params spec)) >>= return . either (\ (_ :: SomeException) -> Nothing) Just
               -- Get the changelog entry from the clean source
               let sourceLog = entry . cleanSource $ target
               let sourceVersion = logVersion sourceLog
               lift (vEPutStrBl 1 ("Released source version: " ++ show oldSrcVersion))
               lift (vEPutStrBl 1 ("Released version: " ++ show oldVersion))
               lift (vEPutStrBl 1 ("Current source version: " ++ show sourceVersion))
               let sourcePackages = aptSourcePackagesSorted poolOS [packageName]
                   buildTrumped = elem packageName (P.buildTrumped params)
               let newVersion = computeNewVersion params sourcePackages (if buildTrumped then Nothing else releaseControlInfo) sourceVersion
               let decision =
                       buildDecision target (P.vendorTag params) (P.oldVendorTags params) (elem packageName (P.forceBuild params))
                                         (P.allowBuildDependencyRegressions params)
                                         oldVersion oldSrcVersion oldRevision oldDependencies releaseStatus
                                         sourceVersion sourceDependencies'
               lift (vEPutStrBl 0 ("Build decision: " ++ show decision))
               -- FIXME: incorporate the release status into the build decision
               case newVersion of
                 Failure messages ->
                    return (Failure messages)
                 Success version ->
                    case decision of
                      Error message -> return (Failure [message])
                      No _ -> return (Success repo)
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
      iStyle = setStyle (appPrefix " ")

appPrefix _ = id

-- |Convert to a simple name and version record to interface with older
-- code.
makeVersion :: BinaryPackage -> PkgVersion
makeVersion package =
    PkgVersion { getName = packageName (packageID package)
               , getVersion = packageVersion (packageID package) }

-- | Build a package and upload it to the local repository.
buildPackage :: (P.RunClass p) => p -> OSImage -> Maybe DebianVersion -> [PkgVersion] -> Maybe String -> [PkgVersion] -> Target -> SourcePackageStatus -> LocalRepository -> ChangeLogEntry -> AptIOT IO (Failing LocalRepository)
buildPackage params cleanOS newVersion oldDependencies sourceRevision sourceDependencies target status repo sourceLog =
    checkDryRun >>
    (lift prepareImage) >>=
    failing (return . Failure) (lift . logEntry) >>=
    failing (return . Failure) (lift . build) >>=
    failing (return . Failure) (liftIO . find) >>=
    failing (return . Failure) upload
    where
      checkDryRun = when (P.dryRun params)  (do lift (vEPutStrBl 0 "Not proceeding due to -n option.")
                                                liftIO (exitWith ExitSuccess))
      prepareImage = prepareBuildImage params cleanOS sourceDependencies buildOS target (P.strictness params)
      logEntry :: DebianBuildTree -> IO (Failing DebianBuildTree)
      logEntry buildTree = 
          case P.noClean params of
            False -> liftIO $ maybeAddLogEntry buildTree newVersion >> return (Success buildTree)
            True -> return (Success buildTree)
      build :: DebianBuildTree -> IO (Failing (DebianBuildTree, NominalDiffTime))
      build buildTree =
          case realSource target of
            Tgt t -> do result <- try (buildPkg params buildOS buildTree status t)
                        case result of
                          Left (e :: SomeException) -> return (Failure [show e])
                          Right elapsed -> return (Success (buildTree, elapsed))
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
                                logDists=[P.buildRelease params],
                                logWho=P.autobuilderEmail params,
                                logDate=date,
                                logComments=
                                    init (logComments sourceLog) ++ changelogText (realSource target) sourceRevision oldDependencies sourceDependencies}
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
                return (setDistribution (P.buildRelease params) changesFile) >>=
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
              [] -> return . Success $ repo
              _ -> return (Failure ["Local upload failed:\n " ++ showErrors (map snd errors)])
      buildOS = Debian.Repo.chrootEnv cleanOS (P.dirtyRoot params)

-- |Prepare the build image by copying the clean image, installing
-- dependencies, and copying the clean source tree.  For a lax build
-- these operations take place in a different order from other types
-- of builds.  For lax: dependencies, then image copy, then source
-- copy.  For other: image copy, then source copy, then dependencies.
prepareBuildImage :: (P.ParamClass p) => p -> OSImage -> [PkgVersion] -> OSImage -> Target -> P.Strictness -> IO (Failing DebianBuildTree)
prepareBuildImage params cleanOS sourceDependencies buildOS target@(Target (Tgt _tgt) _ _ _ _ _ _ _) P.Lax =
    -- Install dependencies directly into the clean environment
    installDependencies cleanOS (cleanSource target) buildDepends sourceDependencies >>=
    failing (return . Failure) (\ x -> prepareTree noClean x)
    where
      prepareTree True _ =
          findOneDebianBuildTree newPath >>=
          return . failing (\ msgs -> Failure (("No build tree at " ++ show newPath) : msgs)) Success
      prepareTree False _ =
          vBOL 0 >>
          vEPutStr 1 "Syncing buildOS" >>
          Debian.Repo.syncEnv cleanOS buildOS >>=
          const (try (copyDebianBuildTree (cleanSource target) newPath)) >>=
          return . either (\ (e :: SomeException) -> Failure [show e]) Success
      buildDepends = (P.buildDepends params)
      noClean = P.noClean params
      newPath = rootPath (rootDir buildOS) ++ fromJust (dropPrefix (rootPath (rootDir cleanOS)) oldPath)
      oldPath = topdir . cleanSource $ target
prepareBuildImage params cleanOS sourceDependencies buildOS target@(Target (Tgt _tgt) _ _ _ _ _ _ _) _ =
    -- Install dependencies directly into the build environment
    findTree noClean >>=
    failing (return . Failure) downloadDeps >>=
    failing (return . Failure) (syncEnv noClean) >>=
    failing (return . Failure) installDeps
    where
      -- findTree :: Bool -> IO (Failing DebianBuildTree)
      findTree False = try (copyDebianBuildTree (cleanSource target) newPath) >>=
                       return . either (\ (e :: SomeException) -> Failure [show e]) Success
      findTree True = findOneDebianBuildTree newPath
      -- downloadDeps :: DebianBuildTree -> IO (Failing DebianBuildTree)
      downloadDeps buildTree = iStyle (downloadDependencies cleanOS buildTree buildDepends sourceDependencies) >>=
                               failing (return . Failure) (const (return (Success buildTree)))
      -- syncEnv :: Bool -> DebianBuildTree -> IO (Failing (OSImage, DebianBuildTree))
      syncEnv False buildTree = vEPutStrBl 1 "Syncing buildOS" >>
                                Debian.Repo.syncEnv cleanOS buildOS >>= (\ os -> return (Success (os, buildTree)))
      syncEnv True buildTree = return (Success (buildOS, buildTree))
      -- installDeps :: (OSImage, DebianBuildTree) -> IO (Failing DebianBuildTree)
      installDeps (buildOS, buildTree) = iStyle (installDependencies buildOS buildTree buildDepends sourceDependencies) >>=
                                         failing (return . Failure) (const (return (Success buildTree)))
      buildDepends = P.buildDepends params
      noClean = P.noClean params
      newPath = rootPath (rootDir buildOS) ++ fromJust (dropPrefix (rootPath (rootDir cleanOS)) (topdir (cleanSource target)))
      iStyle = setStyle (appPrefix " ")
            
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
          concat
          . intersperse "\n"
                $ (["  Source Package Versions: " ++ show (map sourcePackageVersion sourcePackages),
                    "  Required Binary Package Names:"] ++
                   map (("   " ++) . show) (zip (map sourcePackageVersion sourcePackages) (map sourcePackageBinaryNames sourcePackages)) ++
                   missingMessage status ++
                   ["  Binary Package Versions: " ++ show (map binaryPackageVersion binaryPackages),
                    "  Available Binary Packages of Source Package:"] ++
                   map (("   " ++) . show) (zip (map sourcePackageVersion sourcePackages) (map (availableDebNames binaryPackages) sourcePackages)))
      missingMessage Complete = []
      missingMessage (Missing missing) = ["  Missing Binary Package Names: "] ++ map ("   " ++) missing
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
computeNewVersion :: (P.ParamClass p) => p -> [SourcePackage] -> Maybe SourcePackage -> DebianVersion -> Failing DebianVersion
computeNewVersion params
                  available		-- All the versions that exist in the pool in any dist,
					-- the new version number must not equal any of these.
                  current		-- The control file paragraph for the currently uploaded
                                        -- version in this dist.  The new version must be newer
                                        -- than this.
                  sourceVersion =	-- Version number in the changelog entry of the checked-out
                                        -- source code.  The new version must also be newer than this.
    case P.doNotChangeVersion params of
      True -> Success sourceVersion
      False ->
          let vendor = P.vendorTag params
              oldVendors = P.oldVendorTags params
              release = if (P.isDevelopmentRelease params) then
                            Nothing else
                            (Just (sliceName (P.baseRelease params)))
              extra = P.extraReleaseTag params 
              aliases = \ x -> maybe x id (lookup x (P.releaseAliases params)) in
          case parseTag (vendor : oldVendors) sourceVersion of

            (_, Just tag) -> Failure ["Error: the version string in the changelog has a vendor tag (" ++ show tag ++
                                      ".)  This is prohibited because the autobuilder needs to fully control suffixes" ++
                                      " of this form.  This makes it difficult for the author to know what version" ++
                                      " needs to go into debian/changelog to trigger a build by the autobuilder," ++
                                      " particularly since each distribution may have different auto-generated versions."]
            (_, Nothing) -> setTag aliases vendor oldVendors release extra currentVersion (catMaybes . map getVersion $ available) sourceVersion
    where
      getVersion paragraph =
          maybe Nothing (Just . parseDebianVersion . B.unpack) (fieldValue "Version" . sourceParagraph $ paragraph)
      currentVersion =
          maybe Nothing (Just . parseDebianVersion . B.unpack) (maybe Nothing (fieldValue "Version" . sourceParagraph) current)

-- FIXME: Most of this code should move into Debian.Repo.Dependencies
buildDepSolutions' :: [String] -> OSImage -> Relations -> Control -> IO (Failing [(Int, [BinaryPackage])])
buildDepSolutions' preferred os globalBuildDeps debianControl =
    do
      arch <- liftIO $ buildArchOfEnv (rootDir os)
      -- We don't discard any dependencies here even if they are
      -- mentioned in Relax-Depends, that only applies to deciding
      -- whether to build, once we are building we need to install all
      -- the dependencies.  Hence this empty list.
      case G.buildDependencies debianControl of
        Left message -> return (Failure [message])
        Right (_, relations, _) ->
            do let relations' = relations ++ globalBuildDeps
                   relations'' = simplifyRelations packages relations' preferred arch
               -- Do not stare directly into the solutions!  Your head will
               -- explode (because there may be a lot of them.)
               case Debian.Repo.solutions packages (filter (not . alwaysSatisfied) relations'') 100000 of
                 Left error -> message 0 relations' relations'' >> return (Failure [error])
                 Right solutions -> message 2 relations' relations'' >> return (Success solutions)
    where
      alwaysSatisfied xs = any isNothing xs && all isNothing xs
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

-- In ghc610, using readFile on pseudo files in /proc hangs.  Use this instead.
rf path = lazyCommand ("cat '" ++ path ++ "'") L.empty >>= return . (\ (o, _, _) -> o) . collectOutputUnpacked

parseProcCpuinfo :: IO [(String, String)]
parseProcCpuinfo =
    rf "/proc/cpuinfo" >>= return . map makePair . catMaybes . map (matchRegex re) . lines
    where
      re = mkRegex "^(.*[^ \t])[ \t]*:[ \t]*([^ \t].*)$"
      makePair [a, b] = (a, b)
      makePair _ = error "error parsing /proc/cpuinfo"

parseProcMeminfo :: IO [(String, String)]
parseProcMeminfo =
    rf "/proc/meminfo" >>= return . map makePair . catMaybes . map (matchRegex re) . lines
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
    do
      let (Paragraph fields) = changeInfo changes
{-    autobuilderVersion <- processOutput "dpkg -s autobuilder | sed -n 's/^Version: //p'" >>=
                            return . either (const Nothing) Just >>=
                            return . maybe Nothing (listToMaybe . lines) -}
      hostname <- processOutput "hostname" >>= either (\ _ -> return Nothing) (return . listToMaybe . lines)
      cpuInfo <- parseProcCpuinfo
      memInfo <- parseProcMeminfo
      machine <- commandOutput "uname -m" >>= return . listToMaybe . either (const []) lines
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
downloadDependencies :: OSImage -> DebianBuildTree -> [String] -> [PkgVersion] -> IO (Failing [Output])
downloadDependencies os source extra versions =
    do vers <- liftIO (evaluate versions)
       vEPutStrBl 1 . ("versions: " ++) . show $! vers
       (out, codes) <- liftIO (useEnv (rootPath root) forceList' (lazyCommand command L.empty)) >>=
                       vMessage 0 ("Downloading build dependencies into " ++ rootPath (rootDir os)) >>=
                       dotOutput 100 >>= return . partitionResult
       case codes of
         [ExitSuccess] -> return (Success out)
         codes -> vMessage 0 ("FAILURE: " ++ command ++ " -> " ++ show codes ++ "\n" ++ outputToString out) () >>
                  return (Failure ["FAILURE: " ++ command ++ " -> " ++ show codes])
    where
      command = ("export DEBIAN_FRONTEND=noninteractive; " ++
                 (if True then aptGetCommand else pbuilderCommand))
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes --force-yes install -o APT::Install-Recommends=False --download-only " ++ intercalate " " (map showPkgVersion versions ++ extra)
      path = pathBelow (rootPath root) (topdir source)
      root = rootDir os

pathBelow root path =
    maybe (error message) id (dropPrefix root path)
    where message = "Expected a path below " ++ root ++ ", saw " ++ path

-- |Install the package's build dependencies.
installDependencies :: OSImage -> DebianBuildTree -> [String] -> [PkgVersion] -> IO (Failing [Output])
installDependencies os source extra versions =
    do (out, codes) <- liftIO (useEnv (rootPath root) forceList' (lazyCommand command L.empty)) >>=
                       vMessage 0 ("Installing build dependencies into " ++ rootPath (rootDir os)) >>=
                       dotOutput 100 >>= return . partitionResult
       case codes of
         [ExitSuccess] -> return (Success out)
         codes -> vMessage 0 ("FAILURE: " ++ command ++ " -> " ++ show codes ++ "\n" ++ outputToString out) () >>
                  return (Failure ["FAILURE: " ++ command ++ " -> " ++ show codes])
    where
      command = ("export DEBIAN_FRONTEND=noninteractive; " ++
                 (if True then aptGetCommand else pbuilderCommand))
      pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
      aptGetCommand = "apt-get --yes --force-yes install -o APT::Install-Recommends=False " ++ intercalate " " (map showPkgVersion versions ++ extra)
      --aptGetCommand = "apt-get --yes build-dep -o APT::Install-Recommends=False " ++ sourcpackagename
      path = pathBelow (rootPath root) (topdir source)
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
            md5 <- md5sum dscFilePath
            sha1 <- sha1sum dscFilePath
            sha256 <- sha256sum dscFilePath
            case (md5, sha1, sha256) of
              (Success md5, Success sha1, Success sha256) ->
                  do
                    size <- getFileStatus dscFilePath >>= return . fileSize
                    let changes' = changes {changeFiles = (otherFiles ++ [file {changedFileMD5sum = md5, changedFileSHA1sum = sha1, changedFileSHA256sum = sha256, changedFileSize = size}])}
                    Debian.Repo.save changes'
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
      newFieldValue = maybe invalidRevision id revision ++ " " ++ show sourceVersion ++ " " ++ formatVersions versions
      formatVersions versions = intercalate " " (map showPkgVersion versions)
      isDscFile file = isSuffixOf ".dsc" $ changedFileName file

-- | Run a checksum command on a file, return the resulting checksum as text.
doChecksum :: String -> (String -> String) -> FilePath -> IO (Failing String)
doChecksum cmd f path =
    lazyProcess cmd' [path] Nothing Nothing L.empty >>=
    return . either (doError (cmd' ++ " " ++ path)) (Success . f) . toEither . collectOutputUnpacked
    where cmd' = "/usr/bin/" ++ cmd

md5sum = doChecksum "md5sum" (take 32)
sha1sum = doChecksum "sha1sum" (take 40)
sha256sum = doChecksum "sha256sum" (take 64)

toEither x@(text, "", [ExitSuccess]) = Right text
toEither x = Left x

doError cmd (_, s, [ExitSuccess]) = Failure ["Unexpected error output from " ++ cmd ++ ": " ++ show s]
doError cmd (_, _, [ExitFailure n]) = Failure ["Error " ++ show n ++ " running '" ++ cmd ++ "'"]
doError cmd (_, _, _) = Failure ["Internal error running " ++ cmd]

-- |Decide whether to build a package.  We will build if the revision
-- is different from the revision of the uploaded source, or if any of
-- the build dependencies are newer than the versions which were
-- encoded into the uploaded version's control file.
buildDecision :: Target
              -> String
              -> [String]
              -> Bool
              -> Bool
              -> Maybe DebianVersion	-- builtVersion: the version already present in the repository
              -> Maybe DebianVersion	-- builtSrcVersion: the version of the source code that builtVersion was built from
              -> Maybe String		-- builtRevision: that version's revision string
              -> [PkgVersion]		-- builtDependencies: the list of of dependencies for that version
              -> SourcePackageStatus	-- releaseStatus: the status of the version in the repository
              -> DebianVersion		-- sourceVersion: the version number in the newest changelog entry of the source code
              -> [PkgVersion]		-- sourceDependencies: the list of build dependency versions computed from the build environment
              -> BuildDecision
buildDecision target vendorTag oldVendorTags forceBuild allowBuildDependencyRegressions
                  oldVersion oldSrcVersion _oldRevision builtDependencies releaseStatus
                  sourceVersion sourceDependencies =
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
                case compare (dropTag allTags sourceVersion) (dropTag allTags (fromJust oldVersion)) of
                  GT -> Yes ("Source version (" ++ show sourceVersion ++ ") is newer than released source version (" ++ show (fromJust oldVersion) ++ ")")
                  LT -> No ("Source version (" ++ show sourceVersion ++ ") is trumped by released source version (" ++ show (fromJust oldVersion) ++ ")")
                  EQ ->
                      case dropTag allTags sourceVersion == sourceVersion of
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
          case releaseStatus of
            Indep missing | missing /= [] && not (notArchDep (targetControl target)) ->
                  -- The binary packages are missing, we need an arch only build.
                  Arch ("Version " ++ maybe "Nothing" show oldVersion ++ " needs arch only build. (Missing: " ++ show missing ++ ")")
            _ | badDependencies /= [] && not allowBuildDependencyRegressions ->
                  Error ("Build dependency regression (allow with --allow-build-dependency-regressions): " ++ 
                         concat (intersperse ", " (map (\ ver -> show (builtVersion ver) ++ " -> " ++ show ver) badDependencies)))
              | badDependencies /= [] ->
                  Auto ("Build dependency regression: " ++ 
                        concat (intersperse ", " (map (\ ver -> show (builtVersion ver) ++ " -> " ++ show ver) badDependencies)))
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
                  No ("Version " ++ show sourceVersion ++ " of architecture independent package is already in release.")
            Indep missing ->
                  -- The binary packages are missing, we need an arch only build.
                  Arch ("Version " ++ maybe "Nothing" show oldVersion ++ " needs arch only build. (Missing: " ++ show missing ++ ")")
            All ->
                  No ("Version " ++ show sourceVersion ++ " is already in release.")
            _ ->
                  error ("Unexpected releaseStatus: " ++ show releaseStatus)
      notArchDep control =
          all (== "all") . map (maybe "all" (\ (Field (_, s)) -> stripWS s)) . map (lookupP "Architecture") . unControl $ control
      buildDependencyChangeText dependencies =
          "  " ++ intercalate "\n  " lines
          where
            lines = map (\ (built, new) -> show built ++ " -> " ++ show new) (zip builtVersions dependencies)
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
      sourceDependencies' = filter (\ x -> elem (getName x) (packageNames (targetRelaxed target))) sourceDependencies
      -- All the package names mentioned in a dependency list
      packageNames :: G.DepInfo -> [String]
      packageNames (_, deps, _) = nub (map (\ (Rel name _ _) -> name) (concat deps))

----------------------- COPIED FROM GenBuildDeps --------------------
{-

-- Return a list of binary packages which should be ignored for this
-- source package.
ignoredForSourcePackage :: String -> G.RelaxInfo -> [String]
ignoredForSourcePackage source (G.RelaxInfo pairs) =
    catMaybes . map snd . filter (\ (_, x) -> maybe True (== source) x) $ pairs

-- Discard any dependencies not on the filtered package name list.  If
-- this results in an empty list in an or-dep the entire dependency can
-- be discarded.
ignoreDependencies :: [String] -> Relations -> Relations
ignoreDependencies ignore deps =
    filter (/= []) (map (filter keepDep) deps)
    where
      keepDep (Rel name _ _) = not (elem name ignore)

assoc :: String -> Paragraph -> Maybe String
assoc name fields = maybe Nothing (\ (Field (_, v)) -> Just (stripWS v)) (lookupP name fields)

-}
