{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
module Debian.AutoBuilder.Main where

import		 Control.Monad.State
import		 Control.Exception (Exception, try, evaluate)
import		 Control.Monad
import qualified Data.Map as Map
import		 Data.List
import		 Data.Maybe
import qualified Data.Set as Set
import           Data.Time (NominalDiffTime)
import qualified Debian.AutoBuilder.ParamClass as P
import           Debian.AutoBuilder.Params (params, usage)
import qualified Debian.AutoBuilder.ParamRec as R
import		 Debian.AutoBuilder.Target (Target, buildTargets, showTargets, readSpec, targetDocumentation)
import qualified Debian.AutoBuilder.Version as V
import           Debian.Repo.AptImage (prepareAptEnv)
import           Debian.Repo.Cache (updateCacheSources)
import           Debian.Repo.IO (AptIOT, runAptIO, tryAB, setRepoMap, getRepoMap)
import           Debian.Repo.Insert (deleteGarbage)
import           Debian.Repo.LocalRepository (prepareLocalRepository, flushLocalRepository)
import           Debian.Repo.OSImage (buildEssential, prepareEnv)
import           Debian.Repo.Release (prepareRelease)
import           Debian.Repo.Repository (verifyUploadURI, uploadRemote)
import		 Debian.Repo.Slice (appendSliceLists, repoSources, releaseSlices, inexactPathSlices)
import		 Debian.Repo.Types (LocalRepository(..), Layout(Flat), parseSection', releaseRepo,
                                    EnvPath(..), EnvRoot(..), outsidePath, Repository(..),
                                    NamedSliceList(..), SliceName(..), releaseName')
import		 Debian.Shell
import		 Debian.Version
import		 Debian.URI
import		 Extra.Either
import		 Extra.List
import		 Extra.Lock
import		 Extra.Misc
import		 Extra.TIO
import		 System.Unix.Directory hiding (find)
import		 System.Unix.Process
import		 System.Directory
import		 System.Environment
import		 System.Exit
import qualified System.IO as IO
import 	         System.IO.Error (isDoesNotExistError)
import		 System.Posix.Files (removeLink)

-- | Called from the configuration script, this processes a list of
-- parameter sets.
main :: P.ParamClass p => [p] -> IO ()
main [] = error $ "No parameter sets"
main params@(p : _) =
    do doMain (P.verbosity p) (mapM (\ params -> P.buildCache params >>= \ cache -> return (params, cache)) params)

-- |Version of main that uses the configuration file directory and
-- command line parameters.
oldMain :: IO ()
oldMain =
    do verbosity <- getArgs >>= \ args -> return (length (filter (== "-v") args) - length (filter (== "-q") args))
       doMain verbosity (params appName [] doHelp doVersion)

-- | 
doMain :: P.RunClass p => Int -> AptIOT TIO [p] -> IO ()
doMain verbosity f =
    do runTIO (setVerbosity verbosity defStyle) (runAptIO (f >>= mapM doParameterSets) >>= checkResults)
       IO.hFlush IO.stderr

-- |Process one set of parameters.  Usually there is only one, but there
-- can be several which are run sequentially.
doParameterSets :: P.RunClass p => p -> AptIOT TIO (Either Exception (Either Exception (Either String ([Output], NominalDiffTime))))
doParameterSets set =
    withLock (lockFilePath set) (tryAB . runParameterSet $ set)
    where
      lockFilePath params = P.topDir params ++ "/lockfile"

-- |The result of processing a set of parameters is either an
-- exception or a completion code, or, if we fail to get a lock,
-- nothing.  For a single result we can print a simple message,
-- for multiple paramter sets we need to print a summary.
checkResults :: CIO m => [Either Exception (Either Exception (Either String ([Output], NominalDiffTime)))] -> m ()
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

doHelp appName = IO.putStrLn (usage appName ++ targetDocumentation) >> exitWith ExitSuccess
doVersion = IO.putStrLn V.version >> exitWith ExitSuccess

-- |The application name is used to compute the default configuration
-- file names and the name of the cache directory (topDir,) among
-- other things.
appName :: String
appName = "autobuilder"

writeParams p = writeFile "/tmp/params" (show (R.makeParamRec p))

runParameterSet :: P.RunClass p => p -> AptIOT TIO (Either String ([Output], NominalDiffTime))
runParameterSet params =
    do
      liftIO $ writeParams params
      lift doRequiredVersion
      lift doShowParams
      doShowSources
      doFlush
      checkPermissions
      maybe (return ()) (verifyUploadURI (P.doSSHExport $ params)) (P.uploadURI params)
      localRepo <- prepareLocalRepo			-- Prepare the local repository for initial uploads
      cleanOS <- (prepareEnv
                         (P.topDir params)
                         (P.cleanRoot params)
                         buildRelease
                         (Just localRepo)
                         (P.flushRoot params)
                         (P.ifSourcesChanged params)
                         (P.extraEssential params)
                         (P.omitEssential params)
                         (P.extraPackages params ++ ["makedev", "build-essential"]))
      updateCacheSources (P.ifSourcesChanged params) cleanOS

      -- Compute the essential and build essential packages, they will all
      -- be implicit build dependencies.
      globalBuildDeps <- liftIO $ buildEssential cleanOS (P.omitBuildEssential params)
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
      lift (vEPutStrBl 1 "poolSources:" >> setStyle (appPrefix " ") (vEPutStrBl 1 (show (sliceList poolSources))))
      -- Build an apt-get environment which we can use to retrieve all the package lists
      poolOS <- iStyle $ prepareAptEnv (P.topDir params) (P.ifSourcesChanged params) poolSources
      targets <- prepareTargetList 	-- Make a the list of the targets we hope to build
      case partitionEithers targets of
        ([], _ok) ->
            do -- Build all the targets
               buildResult <- buildTargets params cleanOS globalBuildDeps localRepo poolOS (rights targets)
               -- If all targets succeed they may be uploaded to a remote repo
               uploadResult <- upload buildResult
               -- This processes the remote incoming dir
               result <- lift (newDist uploadResult)
               updateRepoCache params
               return result
        (bad, _) ->
            do lift (vEPutStrBl 0 ("Could not prepare source code of some targets:\n " ++ concat (intersperse "\n " bad)))
               return . Left $ "Could not prepare source code of some targets: " ++ concat (intersperse "\n " bad)
    where
      baseRelease =  either (error . show) id (P.findSlice params (P.baseRelease params))
      buildRepoSources = P.buildRepoSources params
      buildReleaseSources = releaseSlices (P.buildRelease params) (inexactPathSlices buildRepoSources)
      buildRelease = NamedSliceList { sliceListName = SliceName (releaseName' (P.buildRelease params))
                                    , sliceList = appendSliceLists [sliceList baseRelease, buildReleaseSources] }
      doRequiredVersion :: CIO m => m ()
      doRequiredVersion =
          case filter (\ (v, _) -> v > parseDebianVersion V.version) (P.requiredVersion params) of
            [] -> return ()
            reasons ->
                do vEPutStrBl 0 ("Version " ++ V.version ++ " is too old:")
                   mapM_ printReason reasons
                   liftIO $ exitWith (ExitFailure 1)                    
          where
            printReason :: CIO m => (DebianVersion, Maybe String) -> m ()
            printReason (v, s) =
                vEPutStr 0 (" Version >= " ++ show v ++ " is required" ++ maybe "" ((++) ":") s)
      doShowParams = when (P.showParams params) (vEPutStr 0 $ "Configuration parameters:\n" ++ P.prettyPrint params)
      doShowSources =
          if (P.showSources params) then
              either (error . show) doShow (P.findSlice params (SliceName (releaseName' (P.buildRelease params)))) else
              return ()
          where
            doShow sources =
                do liftIO . IO.putStrLn $ (sliceName . sliceListName $ sources) ++ ":"
                   liftIO . IO.putStrLn . show . sliceList $ sources
                   liftIO $ exitWith ExitSuccess
      -- FIXME: This may be too late
      doFlush
          | P.flushAll params = 
              do liftIO $ removeRecursiveSafely (P.topDir params)
                 liftIO $ createDirectoryIfMissing True (P.topDir params)
          | True = return ()
      checkPermissions =
          do isRoot <- liftIO $ checkSuperUser
             case isRoot of
               True -> return ()
               False -> do lift (vEPutStr 0 "You must be superuser to run the autobuilder (to use chroot environments.)")
                           liftIO $ exitWith (ExitFailure 1)
      prepareLocalRepo =
          do let path = EnvPath (EnvRoot "") (P.localPoolDir params)
             repo <- prepareLocalRepository path (Just Flat) >>=
                     (if P.flushPool params then flushLocalRepository else return)
             lift (vEPutStrBl 0 $ "Preparing release main in local repository at " ++ outsidePath path)
             release <- prepareRelease repo (P.buildRelease params) [] [parseSection' "main"] (P.archList params)
             let repo' = releaseRepo release
             case repo' of
               LocalRepo repo'' ->
                   case P.cleanUp params of
                     True -> deleteGarbage repo''
                     False -> return repo''
      prepareTargetList =
          do lift (showTargets allTargets)
             lift (vEPutStrBl 0 "Checking all source code out of the repositories:")
             mapStateT (setStyle (appPrefix " ")) (mapM (readSpec params) allTargets)
          where
            allTargets = listDiff (P.targets params) (P.omitTargets params)
            listDiff a b = Set.toList (Set.difference (Set.fromList a) (Set.fromList b))
      upload :: CIO m => (LocalRepository, [Target]) -> AptIOT m [Either String ([Output], NominalDiffTime)]
      upload (repo, [])
          | P.doUpload params =
              case P.uploadURI params of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> lift (vEPutStr 0 "Uploading from local repository") >> uploadRemote repo uri
          | True = return []
      upload (_, failed) =
          do
            lift (vEPutStr 0 ("Some targets failed to build:\n  " ++ consperse "\n  " (map show failed) ++ "\n"))
            case P.doUpload params of
              True -> lift (vEPutStr 0 "Skipping upload.")
              False -> return ()
            liftIO $ exitWith (ExitFailure 1)
      newDist :: CIO m => [Either String ([Output], NominalDiffTime)] -> m (Either String ([Output], NominalDiffTime))
      newDist results
          | P.doNewDist params =
              case P.uploadURI params of
                Just uri ->
                    do vEPutStrBl 1 ("Upload results:\n  " ++ concat (intersperse "\n  " (map show results)))
                       case uriAuthority uri of
                         Just auth ->
                             let cmd = ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++
                                        " " ++ P.newDistProgram params ++ " --root " ++ uriPath uri ++
                                        (concat . map (" --create " ++) . P.createRelease $ params)) in
                             vEPutStr 0 "Running newdist on remote repository" >> runCommandQuietlyTimed cmd
                         Nothing ->
                             let cmd = "newdist --root " ++ uriPath uri in
                             vEPutStr 0 "Running newdist on a local repository" >> runCommandQuietlyTimed cmd
                _ -> error "Missing Upload-URI parameter"
          | True = return (Right ([], (fromInteger 0)))
      iStyle = id {- setStyle (addPrefixes " " " ") -}
      --top = P.topDir params
      --dryRun = P.dryRun params
      -- flush = P.flushSource params
      updateRepoCache :: P.RunClass p => p -> AptIOT TIO ()
      updateRepoCache params =
          do let path = P.topDir params  ++ "/repoCache"
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
            let distros = catMaybes $ map (either (const Nothing) Just . P.findRelease params . fst) dists
            return $ sources ++ concat (map local distros)
    where
      -- FIXME: this is cheating
      localSources distro =
          [DebSourceVerified (DebSource Deb (repoURI localRepo) (Right (show (dist distro), ["main"]))) localRepo,
           DebSourceVerified (DebSource DebSrc (repoURI localRepo) (Right (show (dist distro), ["main"]))) localRepo]
-}