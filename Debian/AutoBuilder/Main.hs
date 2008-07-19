-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
module Debian.AutoBuilder.Main where

import		 Debian.Repo
import		 Debian.Shell
import		 Debian.Version
import		 Debian.URI

import		 Control.Monad.State
import		 Control.Monad.RWS
import		 Extra.TIO
import qualified Debian.Config as Config
import		 Control.Exception
import		 Control.Monad
import qualified Data.Map as Map
import		 Data.List
import		 Data.Maybe
import qualified Data.Set as Set
import		 Extra.Either
import		 Extra.List
import		 Extra.Lock
import		 Extra.Misc
import		 System.Unix.Directory hiding (find)
import		 System.Unix.Process
import qualified Debian.AutoBuilder.Params as Params
import		 System.Directory
import		 System.Environment
import		 System.Exit
import qualified System.IO as IO
import 	         System.IO.Error (isDoesNotExistError)
import		 System.Posix.Files (removeLink)
import		 System.Time
import		 Debian.AutoBuilder.Target
import qualified Debian.AutoBuilder.Version as Version

-- | Convert the command line arguments into a list of flags.  Then
-- expand these flags into a list of flag lists, and then run the
-- application on each list in sequence.  You get multiple flag lists
-- when more than one identifier appears on the right side of a /Use/.
main :: IO ()
main =
    do verbosity <- getArgs >>= \ args -> return (length (filter (== "-v") args) - length (filter (== "-q") args))
       runTIO (setVerbosity verbosity defStyle) (tioMain verbosity)        
       IO.hFlush IO.stderr
    where
      tioMain verbosity =
          runAptIO (aptMain verbosity) >>=
          checkResults
      aptMain verbosity =
          liftIO getArgs >>=
          return . Config.seedFlags appName Params.optSpecs >>=
          liftIO . doHelp >>=
          doVersion >>=
          Params.params verbosity appName >>=
          mapM doParameterSets
      doHelp :: [Config.Flag] -> IO [Config.Flag]
      doHelp flags
          | isJust (Config.findValue flags "Help") =
              do IO.putStrLn (Params.usage appName ++ targetDocumentation) >> exitWith ExitSuccess
          | True = return flags
      doVersion flags
          | isJust (Config.findValue flags "Version") =
              do liftIO (IO.putStrLn Version.version >> exitWith ExitSuccess)
          | True = return flags
      -- Process one set of parameters.  Usually there is only one, but there
      -- can be several which are run sequentially.
      doParameterSets :: Params.Params -> AptIOT TIO (Either Exception (Either Exception (Either String ([Output], TimeDiff))))
      doParameterSets set = withLock (lockFilePath set) (tryAB . runParameterSet $ set)
      lockFilePath params = Params.topDir params ++ "/lockfile"
      -- The result of processing a set of parameters is either an
      -- exception or a completion code, or, if we fail to get a lock,
      -- nothing.  For a single result we can print a simple message,
      -- for multiple paramter sets we need to print a summary.
      checkResults :: CIO m => [Either Exception (Either Exception (Either String ([Output], TimeDiff)))] -> m ()
      checkResults [Right (Left e)] = (vEPutStrBl 0 (show e)) >> liftIO (exitWith $ ExitFailure 1)
      checkResults [Right (Right _)] = eBOL >> (liftIO $ exitWith ExitSuccess)
      checkResults [Left e] = vEPutStrBl 0 ("Failed to obtain lock: " ++ show e ++ "\nAbort.") >> liftIO (exitWith (ExitFailure 1))
      checkResults list =
          do mapM_ (\ (num, result) -> vEPutStrBl 0 ("Parameter set " ++ show num ++ ": " ++ showResult result)) (zip [1..] list)
             eBOL
             case filter isLeft list of
               [] -> liftIO (exitWith ExitSuccess)
               _ -> liftIO (exitWith (ExitFailure 1))
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

runParameterSet :: Params.Params -> AptIOT TIO (Either String ([Output], TimeDiff))
runParameterSet params =
    do
      liftIO doRequiredVersion
      liftIO doShowParams
      doShowSources
      doFlush
      checkPermissions
      maybe (return ()) (verifyUploadURI (Params.doSSHExport $ params)) (Params.uploadURI params)
      localRepo <- prepareLocalRepo			-- Prepare the local repository for initial uploads
      cleanOS <- (prepareEnv
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
      globalBuildDeps <- liftIO $ buildEssential cleanOS (Params.omitBuildEssential params)
      -- Get a list of all sources for the local repository.
      localSources <-
          case localRepo of
            LocalRepository path _ _ ->
                case parseURI ("file://" ++ envPath path) of
                  Nothing -> error $ "Invalid local repo root: " ++ show path
                  Just uri -> repoSources (Just . envRoot $ path) uri
      -- Compute a list of sources for all the releases in the repository we will upload to,
      -- used to avoid creating package versions that already exist.  Also include the sources
      -- for the local repository to avoid collisions there as well.
      let poolSources = NamedSliceList { sliceListName = SliceName (sliceName (sliceListName buildRelease) ++ "-all")
                                       , sliceList = appendSliceLists [buildRepoSources, localSources] }
      liftIO (vEPutStrBl 1 "poolSources:" >> setStyle (appPrefix " ") (vEPutStrBl 1 (show (sliceList poolSources))))
      -- Build an apt-get environment which we can use to retrieve all the package lists
      poolOS <- iStyle $ prepareAptEnv top (Params.ifSourcesChanged params) poolSources
      targets <- prepareTargetList 	-- Make a the list of the targets we hope to build
      case partitionEithers targets of
        ([], ok) ->
            do -- Build all the targets
               buildResult <- buildTargets params cleanOS globalBuildDeps localRepo poolOS (rights targets)
               -- If all targets succeed they may be uploaded to a remote repo
               uploadResult <- upload buildResult
               -- This processes the remote incoming dir
               result <- liftIO (newDist uploadResult)
               updateRepoCache params
               return result
        (bad, _) ->
            do liftIO (vEPutStrBl 0 ("Could not prepare source code of some targets:\n " ++ concat (intersperse "\n " bad)))
               return . Left $ "Could not prepare source code of some targets: " ++ concat (intersperse "\n " bad)
    where
      baseRelease =  either (error . show) id (Params.findSlice params (Params.baseRelease params))
      buildRepoSources = Params.buildRepoSources params
      buildReleaseSources = releaseSlices (Params.buildRelease params) (inexactPathSlices buildRepoSources)
      buildRelease = NamedSliceList { sliceListName = SliceName (releaseName' (Params.buildRelease params))
                                    , sliceList = appendSliceLists [sliceList baseRelease, buildReleaseSources] }
      doRequiredVersion :: CIO m => m ()
      doRequiredVersion =
          case filter (\ (v, _) -> v > parseDebianVersion Version.version) (Params.requiredVersion params) of
            [] -> return ()
            reasons ->
                do vEPutStrBl 0 ("Version " ++ Version.version ++ " is too old:")
                   mapM_ printReason reasons
                   liftIO $ exitWith (ExitFailure 1)                    
          where
            printReason :: CIO m => (DebianVersion, Maybe String) -> m ()
            printReason (v, s) =
                vEPutStr 0 (" Version >= " ++ show v ++ " is required" ++ maybe "" ((++) ":") s)
      doShowParams = when (Params.showParams params) (vEPutStr 0 $ "Configuration parameters:\n" ++ Params.prettyPrint params)
      doShowSources =
          if (Params.showSources params) then
              either (error . show) doShow (Params.findSlice params (SliceName (releaseName' (Params.buildRelease params)))) else
              return ()
          where
            doShow sources =
                do liftIO . IO.putStrLn $ (sliceName . sliceListName $ sources) ++ ":"
                   liftIO . IO.putStrLn . show . sliceList $ sources
                   liftIO $ exitWith ExitSuccess
      -- FIXME: This may be too late
      doFlush
          | Params.flushAll params = 
              do liftIO $ removeRecursiveSafely top
                 liftIO $ createDirectoryIfMissing True top
          | True = return ()
      checkPermissions =
          do isRoot <- liftIO $ checkSuperUser
             case isRoot of
               True -> return ()
               False -> do liftIO (vEPutStr 0 "You must be superuser to run the autobuilder (to use chroot environments.)")
                           liftIO $ exitWith (ExitFailure 1)
      prepareLocalRepo =
          do let path = EnvPath (EnvRoot "") (Params.localPoolDir params)
             repo <- prepareLocalRepository path (Just Flat) >>=
                     (if Params.flushPool params then flushLocalRepository else return)
             liftIO (vEPutStrBl 0 $ "Preparing release main in local repository at " ++ outsidePath path)
             release <- prepareRelease repo (Params.buildRelease params) [] [parseSection' "main"] (Params.archList params)
             let repo' = releaseRepo release
             case repo' of
               LocalRepo repo'' ->
                   case Params.cleanUp params of
                     True -> deleteGarbage repo''
                     False -> return repo''
      prepareTargetList =
          do liftIO (showTargets allTargets)
             liftIO (vEPutStrBl 0 "Checking all source code out of the repositories:")
             mapRWST (setStyle (appPrefix " "))
                         (mapM (readSpec (Params.debug params) top flush
                                (Params.ifSourcesChanged params) (Params.allSources params)) allTargets)
          where
            allTargets = listDiff (Params.targets params) (Params.omitTargets params)
            listDiff a b = Set.toList (Set.difference (Set.fromList a) (Set.fromList b))
      upload :: CIO m => (LocalRepository, [Target]) -> AptIOT m [Either String ([Output], TimeDiff)]
      upload (repo, [])
          | Params.doUpload params =
              case Params.uploadURI params of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> liftIO (vEPutStr 0 "Uploading from local repository") >> uploadRemote repo uri
          | True = return []
      upload (_, failed) =
          do
            liftIO (vEPutStr 0 ("Some targets failed to build:\n  " ++ consperse "\n  " (map show failed) ++ "\n"))
            case Params.doUpload params of
              True -> liftIO (vEPutStr 0 "Skipping upload.")
              False -> return ()
            liftIO $ exitWith (ExitFailure 1)
      newDist :: CIO m => [Either String ([Output], TimeDiff)] -> m (Either String ([Output], TimeDiff))
      newDist results
          | Params.doNewDist params =
              case Params.uploadURI params of
                Just uri ->
                    do vEPutStrBl 1 ("Upload results:\n  " ++ concat (intersperse "\n  " (map show results)))
                       case uriAuthority uri of
                         Just auth ->
                             let cmd = ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++
                                        " " ++ Params.newDistProgram params ++ " --root " ++ uriPath uri ++
                                        (concat . map (" --create " ++) . Params.createRelease $ params)) in
                             vEPutStr 0 "Running newdist on remote repository" >> runCommandQuietlyTimed cmd
                         Nothing ->
                             let cmd = "newdist --root " ++ uriPath uri in
                             vEPutStr 0 "Running newdist on a local repository" >> runCommandQuietlyTimed cmd
                _ -> error "Missing Upload-URI parameter"
          | True = return (Right ([], noTimeDiff))
      iStyle = id {- setStyle (addPrefixes " " " ") -}
      top = Params.topDir params
      --dryRun = Params.dryRun params
      flush = Params.flushSource params
      updateRepoCache :: Params.Params -> AptIOT TIO ()
      updateRepoCache params =
          do let path = Params.topDir params  ++ "/repoCache"
             live <- get >>= return . getRepoMap
             cache <- liftIO $ loadCache path
             --tio (hPutStrBl IO.stderr (show (Map.toList live)))
             let merged = show . map (\ (uri, x) -> (show uri, x)) . Map.toList $ Map.union live cache
             --tio (hPutStrBl IO.stderr merged)
             liftIO (removeLink path `Prelude.catch` (\e -> unless (isDoesNotExistError e) (ioError e))) >> liftIO (writeFile path merged)
             return ()
          where
            isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map URI (Maybe Repository))
            loadCache path =
                do text <- try (readFile path) >>= return . either (const "") id 
                   pairs <- try (evaluate (read text)) >>= return . either (const []) id
                   let (pairs' :: [(URI, Maybe Repository)]) =
                           catMaybes (map (\ (s, x) -> case parseURI s of
                                                         Nothing -> Nothing
                                                         Just uri -> Just (uri, x)) pairs)
                   return . Map.fromList . filter isRemote $ pairs'

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