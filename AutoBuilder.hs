-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
--
-- * "Extra"
--
-- * "VersionPolicy"
--
-- * "Version"
--
-- * "Config"
--
-- * "DryRun"
--
-- * "BuildTarget"
--
-- * "Params"
--
-- * "Target"
--
module Main where

import		 Debian.AptImage
import		 Debian.Cache
import		 Debian.IO
import		 Debian.Local.Insert
import		 Debian.Local.Release
import		 Debian.Local.Repo
import qualified Debian.OSImage as OSImage
import		 Debian.Repo
import		 Debian.Slice
import		 Debian.Types
import qualified Config
import		 Control.Exception
import		 Control.Monad
import		 Data.List
import		 Data.Maybe
import qualified Data.Set as Set
import		 Extra.Bool
import		 Extra.List
import		 Extra.Lock
import		 Extra.Misc
import		 Debian.Version
import		 Linspire.Unix.Directory hiding (find)
import		 Network.URI
import qualified Params
import		 System.Directory
import		 System.Environment
import		 System.Exit
import qualified System.IO as IO
import		 System.Time
import		 Target
import qualified Version

-- | Convert the command line arguments into a list of flags.  Then
-- expand these flags into a list of flag lists, and then run the
-- application on each list in sequence.  You get multiple flag lists
-- when more than one identifier appears on the right side of a /Use/.
main :: IO ()
main =
    do verbosity <- getArgs >>= return . length . filter (== "-v")
       run (Debian.IO.defStyle verbosity)
               (io getArgs >>=
                return . Config.seedFlags appName Params.optSpecs >>=
                doHelp >>= doVersion >>=
                Params.params verbosity appName >>=
                mapM doParams >>=
                setStyle (normalStyle IO.stdout . normalStyle IO.stderr) . checkResults >>
                vBOL 0)
       IO.hFlush IO.stderr
    where
      doHelp flags
          | isJust (Config.findValue flags "Help") = do io (IO.putStrLn (Params.usage appName ++ targetDocumentation) >> exitWith ExitSuccess)
          | True = return flags
      doVersion flags
          | isJust (Config.findValue flags "Version") = do io (IO.putStrLn Version.version >> exitWith ExitSuccess)
          | True = return flags
      doParams params = withLock (lockFilePath params) (setStyle (Params.style params) . tryAB . runParams $ params)
      lockFilePath params = Params.topDir params ++ "/lockfile"
      -- The result of processing a set of parameters is either an
      -- exception or a completion code, or, if we fail to get a lock,
      -- nothing.  For a single result we can print a simple message,
      -- for multiple paramter sets we need to print a summary.
      checkResults :: [Either Exception (Either Exception TaskElapsed)] -> AptIO ()
      checkResults [Right (Left e)] = setStyle (normalStyle IO.stdout . normalStyle IO.stderr) (msgLn 0 (show e)) >> io (exitWith $ ExitFailure 1)
      checkResults [Right (Right _)] = io $ exitWith ExitSuccess
      checkResults [Left e] = msgLn 0 ("Failed to obtain lock: " ++ show e ++ "\nAbort.") >> io (exitWith (ExitFailure 1))
      checkResults list =
          do mapM_ (\ (num, result) -> msgLn 0 ("Parameter set " ++ show num ++ ": " ++ showResult result)) (zip [1..] list)
             case filter isLeft list of [] -> io (exitWith ExitSuccess); _ -> io (exitWith (ExitFailure 1))
             where showResult (Right (Left e)) = show e
                   showResult (Right (Right _)) = "Ok"
                   showResult (Left e) = "Ok (" ++ show e ++ ")"
                   isLeft (Right (Left _)) = True
                   isLeft (Left _) = True
                   isLeft (Right (Right _)) = False

-- |The application name is used to compute the default configuration
-- file names and the name of the cache directory (topDir,) among
-- other things.
appName :: String
appName = "autobuilder"

runParams :: Params.Params -> AptIO TaskElapsed
runParams params =
    do
      doRequiredVersion
      doShowParams
      doShowSources
      doFlush
      checkPermissions
      maybe (return ()) (verifyUploadURI (Params.doSSHExport $ params)) (Params.uploadURI params)
      localRepo <- prepareLocalRepo			-- Prepare the local repository for initial uploads
      --msgLn ("Local repository: " ++ show localRepo)
      let baseRelease =  either (error . show) id (Params.findSlice params (Params.baseRelease params))
      let buildRepoSources = Params.buildRepoSources params
      let buildReleaseSources = releaseSlices (Params.buildRelease params) (inexactPathSlices buildRepoSources)
      let buildRelease = NamedSliceList { sliceListName = SliceName (relName (Params.buildRelease params))
                                        , sliceList = appendSliceLists [sliceList baseRelease, buildReleaseSources] }
{-
      buildRelease <-
          releaseFromConfig UpdateSources top (SliceName buildRelease) (appendSliceLists [baseRelease, buildReleaseSources])
-}
      cleanOS <- (OSImage.prepareEnv
                         top
                         (Params.cleanRoot params)
                         buildRelease
                         (Just localRepo)
                         (Params.flushRoot params)
                         (Params.ifSourcesChanged params)
                         (Params.extraEssential params)
                         (Params.omitEssential params)
                         (Params.extraPackages params ++ ["makedev", "build-essential"]))
      updateCacheSources (Params.ifSourcesChanged params) cleanOS

      -- Compute the essential and build essential packages, they will all
      -- be implicit build dependencies.
      globalBuildDeps <- io $ OSImage.buildEssential cleanOS (Params.omitBuildEssential params)
      -- Get a list of all sources for the local repository.
      localSources <-
          case localRepo of
            LocalRepo path _ _ ->
                case parseURI ("file://" ++ envPath path) of
                  Nothing -> error $ "Invalid local repo root: " ++ show path
                  Just uri -> repoSources (Just . envRoot $ path) uri
      -- Compute a list of sources for all the releases in the repository we will upload to,
      -- used to avoid creating package versions that already exist.  Also include the sources
      -- for the local repository to avoid collisions there as well.
      let poolSources = NamedSliceList { sliceListName = SliceName (sliceName (sliceListName buildRelease) ++ "-all")
                                       , sliceList = appendSliceLists [buildRepoSources, localSources] }
      vPutStrLn 1 $ "poolSources:\n  " ++ show (sliceList poolSources)
      -- Build an apt-get environment which we can use to retrieve all the package lists
      poolOS <- (iStyle . debugStyle) $ prepareAptEnv top (Params.ifSourcesChanged params) poolSources
      targets <- prepareTargetList 	-- Make a the list of the targets we hope to build
      buildResult <- buildTargets params cleanOS globalBuildDeps localRepo poolOS targets	-- Build all the targets
      uploadResult <- uploadStyle . upload $ buildResult	-- If all targets succeed they may be uploaded to a remote repo
      newdistStyle . newDist $ uploadResult		-- This processes the remote incoming dir
    where
      ReleaseName buildRelease = Params.buildRelease params
      doRequiredVersion =
          case filter (\ (v, _) -> v > parseDebianVersion Version.version) (Params.requiredVersion params) of
            [] -> return ()
            reasons ->
                do vPutStrLn 0 ("Version " ++ Version.version ++ " is too old:")
                   mapM_ printReason reasons
                   io $ exitWith (ExitFailure 1)                    
          where
            printReason (v, s) =
                vPutStr 0 (" Version >= " ++ show v ++ " is required" ++ maybe "" ((++) ":") s)
      doShowParams = when (Params.showParams params) (vPutStr 0 $ "Configuration parameters:\n" ++ Params.prettyPrint params)
      doShowSources =
          if (Params.showSources params) then
              either (error . show) doShow (Params.findSlice params (SliceName buildRelease)) else
              return ()
          where
            doShow sources =
                do io . IO.putStrLn $ (sliceName . sliceListName $ sources) ++ ":"
                   io . IO.putStrLn . show . sliceList $ sources
                   io $ exitWith ExitSuccess
      -- FIXME: This may be too late
      doFlush
          | Params.flushAll params = 
              do io $ removeRecursiveSafely top
                 io $ createDirectoryIfMissing True top
          | True = return ()
      checkPermissions =
          do isRoot <- io $ checkSuperUser
             case isRoot of
               True -> return ()
               False -> do vPutStr 0 "You must be superuser to run the autobuilder (to use chroot environments.)"
                           io $ exitWith (ExitFailure 1)
      prepareLocalRepo =
          do let path = EnvPath (EnvRoot "") (Params.localPoolDir params)
             vPutStrLn 0 $ "Preparing local repository at " ++ show path
             repo <- prepareLocalRepository path (Just Flat) >>=
                     (if Params.flushPool params then flushLocalRepository else return)
             vPutStrLn 0 $ "Preparing release main in local repository at " ++ show path
             release <- runStyle $ prepareRelease repo (Params.buildRelease params) [] [Section "main"] (Params.archList params)
             let repo' = releaseRepo release
             case Params.cleanUp params of
               True -> runStyle $ deleteGarbage repo'
               False -> return repo'
      prepareTargetList =
          do showTargets allTargets
             msgLn 0 "Checking all source code out of the repositories:"
             (setStyle (addPrefixes " " " ") $
                       mapM (debugStyle . Target.readSpec (Params.debug params) top flush
                                            (Params.ifSourcesChanged params) (Params.allSources params)) allTargets)
{-
             case result of
               Left e -> eBOL >> io (IO.hPutStrLn IO.stderr "error!") >> error (show e)
               Right x -> return x
-}
          where
            allTargets = listDiff (Params.targets params) (Params.omitTargets params)
            listDiff a b = Set.toList (Set.difference (Set.fromList a) (Set.fromList b))
      upload :: (LocalRepo, [Target]) -> AptIO [TaskElapsed]
      upload (repo, [])
          | Params.doUpload params =
              case Params.uploadURI params of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> uploadRemote repo uri
          | True = return []
      upload (_, failed) =
          do
            vPutStr 0 ("Some targets failed to build:\n  " ++ consperse "\n  " (map show failed) ++ "\n")
            case Params.doUpload params of
              True -> vPutStr 0 "Skipping upload."
              False -> return ()
            io $ exitWith (ExitFailure 1)
      newDist :: [TaskElapsed] -> AptIO (TaskElapsed)
      newDist results
          | Params.doNewDist params =
              case Params.uploadURI params of
                Just uri ->
                    do vPutStrLn 1 ("Upload results:\n  " ++ concat (intersperse "\n  " (map show results)))
                       case uriAuthority uri of
                         Just auth ->
                             let cmd = ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++
                                        " " ++ Params.newDistProgram params ++ " --root " ++ uriPath uri ++
                                        (concat . map (" --create " ++) . Params.createRelease $ params)) in
                             iStyle $ systemTask'_ cmd
                         Nothing ->
                             let cmd = "newdist --root " ++ uriPath uri in
                             iStyle $ systemTask'_ cmd
                _ -> error "Missing Upload-URI parameter"
          | True = return (ExitSuccess, noTimeDiff)
      iStyle = setStyle (addPrefixes " " " ")
      top = Params.topDir params
      debugStyle = setStyle (cond Debian.IO.dryRun Debian.IO.realRun debug)
      debug = Params.debug params
      uploadStyle = setStyle $ (setStart (Just "Uploading from local repository")) . (cond Debian.IO.dryRun Debian.IO.realRun dryRun)
      newdistStyle = setStyle $ (setStart (Just "Running newdist on remote repository") .
                                 (cond Debian.IO.dryRun Debian.IO.realRun dryRun) .
                                 setPrefixes " " " " .
                                 setEcho True)
      runStyle = setStyle (cond Debian.IO.dryRun Debian.IO.realRun dryRun)
      dryRun = Params.dryRun params
      flush = Params.flushSource params

-- | Return the sources.list for the union of all the dists in the
-- upload repository plus the local repository.  This is used to
-- find an unused version number for the package being built.
{-
localRepoSources :: Repository -> ReleaseName -> IO [DebSourceVerified]
findPoolSources localRepo dist =
    case (repoRoot localRepo, repoReleaseNames localRepo) of
      (Right _, _) -> error "Local repository is remote!"
      (Left root, dists) ->
          do
            let sources = ReleaseCache.sources distro
            let distros = catMaybes $ map (either (const Nothing) Just . Params.findRelease params . fst) dists
            return $ sources ++ concat (map local distros)
    where
      -- FIXME: this is cheating
      localSources distro =
          [DebSourceVerified (DebSource Deb (repoURI localRepo) (Right (show (dist distro), ["main"]))) localRepo,
           DebSourceVerified (DebSource DebSrc (repoURI localRepo) (Right (show (dist distro), ["main"]))) localRepo]
-}