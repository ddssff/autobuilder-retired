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

import		 Control.Monad.State
import		 Debian.AptImage
import		 Debian.Cache
import		 Debian.IO
import		 Debian.TIO
import		 Debian.Shell
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
import qualified Data.Map as Map
import		 Data.List
import		 Data.Maybe
import qualified Data.Set as Set
import		 Extra.Either
import		 Extra.List
import		 Extra.Lock
import		 Extra.Misc
import		 Debian.Version
import		 Linspire.Unix.Directory hiding (find)
import		 Linspire.Unix.Process
import		 Ugly.URI
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
    do verbosity <- getArgs >>= \ args -> return (length (filter (== "-v") args) - length (filter (== "-q") args))
       runTIO (setVerbosity verbosity defStyle) (tioMain verbosity)        
       IO.hFlush IO.stderr
    where
      tioMain verbosity =
          run Debian.IO.aptIOStyle (aptMain verbosity) >>=
          checkResults
      aptMain verbosity =
          io getArgs >>=
          return . Config.seedFlags appName Params.optSpecs >>=
          io . doHelp >>=
          doVersion >>=
          Params.params verbosity appName >>=
          mapM doParams
      doHelp :: [Config.Flag] -> IO [Config.Flag]
      doHelp flags
          | isJust (Config.findValue flags "Help") =
              do IO.putStrLn (Params.usage appName ++ targetDocumentation) >> exitWith ExitSuccess
          | True = return flags
      doVersion flags
          | isJust (Config.findValue flags "Version") =
              do io (IO.putStrLn Version.version >> exitWith ExitSuccess)
          | True = return flags
      -- Process one set of parameters.  Usually there is only one, but there
      -- can be several which are run sequentially.
      doParams :: Params.Params -> AptIO (Either Exception (Either Exception (Either String ([Output], TimeDiff))))
      doParams parameterSet = withLock (lockFilePath parameterSet) (tryAB . runParams $ parameterSet)
      lockFilePath params = Params.topDir params ++ "/lockfile"
      -- The result of processing a set of parameters is either an
      -- exception or a completion code, or, if we fail to get a lock,
      -- nothing.  For a single result we can print a simple message,
      -- for multiple paramter sets we need to print a summary.
      checkResults :: [Either Exception (Either Exception (Either String ([Output], TimeDiff)))] -> TIO ()
      checkResults [Right (Left e)] = (msgLn 0 (show e)) >> lift (exitWith $ ExitFailure 1)
      checkResults [Right (Right _)] = hBOL IO.stderr >> (lift $ exitWith ExitSuccess)
      checkResults [Left e] = msgLn 0 ("Failed to obtain lock: " ++ show e ++ "\nAbort.") >> lift (exitWith (ExitFailure 1))
      checkResults list =
          do mapM_ (\ (num, result) -> msgLn 0 ("Parameter set " ++ show num ++ ": " ++ showResult result)) (zip [1..] list)
             hBOL IO.stderr
             case filter isLeft list of
               [] -> lift (exitWith ExitSuccess)
               _ -> lift (exitWith (ExitFailure 1))
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

runParams :: Params.Params -> AptIO (Either String ([Output], TimeDiff))
runParams params =
    do
      tio doRequiredVersion
      tio doShowParams
      doShowSources
      doFlush
      checkPermissions
      maybe (return ()) (verifyUploadURI (Params.doSSHExport $ params)) (Params.uploadURI params)
      localRepo <- prepareLocalRepo			-- Prepare the local repository for initial uploads
      let baseRelease =  either (error . show) id (Params.findSlice params (Params.baseRelease params))
      let buildRepoSources = Params.buildRepoSources params
      let buildReleaseSources = releaseSlices (Params.buildRelease params) (inexactPathSlices buildRepoSources)
      let buildRelease = NamedSliceList { sliceListName = SliceName (releaseName' (Params.buildRelease params))
                                        , sliceList = appendSliceLists [sliceList baseRelease, buildReleaseSources] }
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
            LocalRepository path _ _ ->
                case parseURI ("file://" ++ envPath path) of
                  Nothing -> error $ "Invalid local repo root: " ++ show path
                  Just uri -> repoSources (Just . envRoot $ path) uri
      -- Compute a list of sources for all the releases in the repository we will upload to,
      -- used to avoid creating package versions that already exist.  Also include the sources
      -- for the local repository to avoid collisions there as well.
      let poolSources = NamedSliceList { sliceListName = SliceName (sliceName (sliceListName buildRelease) ++ "-all")
                                       , sliceList = appendSliceLists [buildRepoSources, localSources] }
      tio (vPutStrBl 1 "poolSources:" >> setStyle (appPrefix " ") (vPutStrBl 1 (show (sliceList poolSources))))
      -- Build an apt-get environment which we can use to retrieve all the package lists
      poolOS <- iStyle $ prepareAptEnv top (Params.ifSourcesChanged params) poolSources
      targets <- prepareTargetList 	-- Make a the list of the targets we hope to build
      -- Build all the targets
      buildResult <- buildTargets params cleanOS globalBuildDeps localRepo poolOS (rights targets)
      -- If all targets succeed they may be uploaded to a remote repo
      uploadResult <- upload buildResult
      -- This processes the remote incoming dir
      result <- tio (newDist uploadResult)
      updateRepoCache params
      return result
    where
      buildRelease = Params.buildRelease params
      doRequiredVersion =
          case filter (\ (v, _) -> v > parseDebianVersion Version.version) (Params.requiredVersion params) of
            [] -> return ()
            reasons ->
                do vPutStrBl 0 ("Version " ++ Version.version ++ " is too old:")
                   mapM_ printReason reasons
                   lift $ exitWith (ExitFailure 1)                    
          where
            printReason (v, s) =
                vPutStr 0 (" Version >= " ++ show v ++ " is required" ++ maybe "" ((++) ":") s)
      doShowParams = when (Params.showParams params) (vPutStr 0 $ "Configuration parameters:\n" ++ Params.prettyPrint params)
      doShowSources =
          if (Params.showSources params) then
              either (error . show) doShow (Params.findSlice params (SliceName (releaseName' buildRelease))) else
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
               False -> do tio (vPutStr 0 "You must be superuser to run the autobuilder (to use chroot environments.)")
                           io $ exitWith (ExitFailure 1)
      prepareLocalRepo =
          do let path = EnvPath (EnvRoot "") (Params.localPoolDir params)
             repo <- prepareLocalRepository path (Just Flat) >>=
                     (if Params.flushPool params then flushLocalRepository else return)
             tio (vPutStrBl 0 $ "Preparing release main in local repository at " ++ outsidePath path)
             release <- prepareRelease repo (Params.buildRelease params) [] [parseSection' "main"] (Params.archList params)
             let repo' = releaseRepo release
             case repo' of
               LocalRepo repo'' ->
                   case Params.cleanUp params of
                     True -> deleteGarbage repo''
                     False -> return repo''
      prepareTargetList =
          do tio (showTargets allTargets)
             tio (msgLn 0 "Checking all source code out of the repositories:")
             mapM (Target.readSpec (Params.debug params) top flush
                             (Params.ifSourcesChanged params) (Params.allSources params)) allTargets
          where
            allTargets = listDiff (Params.targets params) (Params.omitTargets params)
            listDiff a b = Set.toList (Set.difference (Set.fromList a) (Set.fromList b))
      upload :: (LocalRepository, [Target]) -> AptIO [Either String ([Output], TimeDiff)]
      upload (repo, [])
          | Params.doUpload params =
              case Params.uploadURI params of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> tio (vPutStr 0 "Uploading from local repository") >> uploadRemote repo uri
          | True = return []
      upload (_, failed) =
          do
            tio (vPutStr 0 ("Some targets failed to build:\n  " ++ consperse "\n  " (map show failed) ++ "\n"))
            case Params.doUpload params of
              True -> tio (vPutStr 0 "Skipping upload.")
              False -> return ()
            io $ exitWith (ExitFailure 1)
      newDist :: [Either String ([Output], TimeDiff)] -> TIO (Either String ([Output], TimeDiff))
      newDist results
          | Params.doNewDist params =
              case Params.uploadURI params of
                Just uri ->
                    do vPutStrBl 1 ("Upload results:\n  " ++ concat (intersperse "\n  " (map show results)))
                       case uriAuthority uri of
                         Just auth ->
                             let cmd = ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++
                                        " " ++ Params.newDistProgram params ++ " --root " ++ uriPath uri ++
                                        (concat . map (" --create " ++) . Params.createRelease $ params)) in
                             vPutStr 0 "Running newdist on remote repository" >> runCommandQuietlyTimed cmd
                         Nothing ->
                             let cmd = "newdist --root " ++ uriPath uri in
                             vPutStr 0 "Running newdist on a local repository" >> runCommandQuietlyTimed cmd
                _ -> error "Missing Upload-URI parameter"
          | True = return (Right ([], noTimeDiff))
      iStyle = id {- setStyle (addPrefixes " " " ") -}
      top = Params.topDir params
      --dryRun = Params.dryRun params
      flush = Params.flushSource params
      updateRepoCache params =
          do let path = Params.topDir params  ++ "/repoCache"
             live <- getRepoMap
             cache <- io $ loadCache path
             --tio (hPutStrBl IO.stderr (show (Map.toList live)))
             let merged = show . Map.toList $ Map.union live cache
             --tio (hPutStrBl IO.stderr merged)
             io $ evaluate merged >>= writeFile path
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