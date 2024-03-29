{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
module Debian.AutoBuilder.Main 
    ( main
    ) where

import Control.Arrow (first)
import Control.Applicative ((<$>))
import Control.Applicative.Error (Failing(..), maybeRead)
import Control.Exception(Exception, SomeException, try, catch, AsyncException(UserInterrupt), fromException)
import Control.Monad(foldM, when, unless)
import Control.Monad.Error (MonadError, catchError)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State(MonadIO(..), MonadTrans(..), MonadState(get), StateT, runStateT)
import qualified Data.ByteString.Lazy as L
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.List(intercalate)
import Data.Maybe(catMaybes, fromMaybe)
import Data.Time(NominalDiffTime)
import Debian.AutoBuilder.BuildTarget (retrieve)
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Target(buildTargets, showTargets)
import Debian.AutoBuilder.Types.Buildable (Target, targetName, asBuildable)
import qualified Debian.AutoBuilder.Types.CacheRec as C
import Debian.AutoBuilder.Types.Download (Download)
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Version as V
import Debian.Release (parseSection', releaseName')
import Debian.Repo.Monads.Top (askTop)
import Debian.Sources (SliceName(..))
import Debian.Repo.AptImage(prepareAptEnv)
import Debian.Repo.Cache(updateCacheSources)
import Debian.Repo.Insert(deleteGarbage)
import Debian.Repo.Monads.AptState (AptState, initState, getRepoMap)
import Debian.Repo.Monads.MonadApt (MonadApt(getApt))
import Debian.Repo.Monads.MonadDeb (MonadDeb)
import Debian.Repo.Monads.Top (sub)
import Debian.Repo.LocalRepository(prepareLocalRepository, flushLocalRepository)
import Debian.Repo.OSImage(OSImage, buildEssential, prepareEnv, chrootEnv)
import Debian.Repo.Release(prepareRelease)
import Debian.Repo.Repository(uploadRemote, verifyUploadURI)
import Debian.Repo.Slice(appendSliceLists, inexactPathSlices, releaseSlices, repoSources)
import Debian.Repo.Types(EnvRoot(EnvRoot), EnvPath(..),
                         Layout(Flat), Release(releaseRepo),
                         NamedSliceList(..), Repository(LocalRepo),
                         LocalRepository(LocalRepository), outsidePath)
import Debian.URI(URIAuth(uriUserInfo, uriRegName), URI(uriScheme, uriPath, uriAuthority), parseURI)
import Debian.Version(DebianVersion, parseDebianVersion, prettyDebianVersion)
import Extra.Lock(withLock)
import Extra.Misc(checkSuperUser)
import Prelude hiding (catch)
import System.Directory(createDirectoryIfMissing)
import System.Posix.Files(removeLink)
import System.Exit(ExitCode(..), exitWith)
import qualified System.IO as IO
import System.IO.Error(isDoesNotExistError)
import System.Process (CmdSpec(..))
import System.Process.Progress (Output, timeTask, runProcessF, withModifiedVerbosity, quieter, qPutStrLn, qPutStr, ePutStrLn, ePutStr)
import System.Unix.Directory(removeRecursiveSafely)
import Text.Printf ( printf )
import Text.PrettyPrint.Class (pretty)

-- | Called from the configuration script, this processes a list of
-- parameter sets.
main :: [(P.ParamRec, P.Packages)] -> IO ()
main [] = error "No parameter sets"
main paramSets = do
  -- Do parameter sets until there is a failure.
  (results, _) <- runStateT (foldM (\ results (params, pkgs) ->
                                        do top <- liftIO $ P.computeTopDir params
                                           runReaderT (doParameterSet results (params, pkgs)) top) [] paramSets) initState
  IO.hFlush IO.stdout
  IO.hFlush IO.stderr
  -- The result of processing a set of parameters is either an
  -- exception or a completion code.  Here we print a summary and
  -- exit with a suitable result code.
  -- ePutStrLn (intercalate "\n  " (map (\ (num, result) -> "Parameter set " ++ show num ++ ": " ++ showResult result) (zip [(1 :: Int)..] results)))
  case partitionFailing results of
    ([], _) -> return ()
    _ ->
        ePutStrLn (intercalate "\n  " (map (\ (num, result) -> "Parameter set " ++ show num ++ ": " ++ showResult result) (zip [(1 :: Int)..] results))) >>
        exitWith (ExitFailure 1)
    where showResult (Failure ss) = intercalate "\n  " ("Failure:" : ss)
          showResult (Success _) = "Ok"
          partitionFailing :: [Failing a] -> ([[String]], [a])
          partitionFailing xs = p ([], []) xs
              where p result [] = result
                    p (fs, ss) (Failure f : more) = p (f : fs, ss) more
                    p (fs, ss) (Success s : more) = p (fs, s : ss) more

-- |Process one set of parameters.  Usually there is only one, but there
-- can be several which are run sequentially.  Stop on first failure.
doParameterSet :: MonadDeb e m => [Failing ([Output L.ByteString], NominalDiffTime)] -> (P.ParamRec, P.Packages) -> m [Failing ([Output L.ByteString], NominalDiffTime)]
doParameterSet results (params, packages)
    | any isFailure results = return results
    where
      isFailure (Failure _) = True
      isFailure _ = False
doParameterSet results (params, packages) =
    (do quieter (- (P.verbosity params)) $
          sub "lockfile" >>= \ path -> withLock path $
            do cacheRec <- quieter 2 (P.buildCache params packages)
               result <- runParameterSet cacheRec
               return (result : results)) `catchError` (\ e -> return (Failure [show e] : results))

runParameterSet :: MonadDeb e m => C.CacheRec -> m (Failing ([Output L.ByteString], NominalDiffTime))
runParameterSet cache =
    askTop >>= \ top ->
    do
      liftIO doRequiredVersion
      when (P.showParams params) (withModifiedVerbosity (const 1) (liftIO doShowParams))
      when (P.showSources params) (withModifiedVerbosity (const 1) (liftIO doShowSources))
      when (P.flushAll params) doFlush
      liftIO checkPermissions
      maybe (return ()) (verifyUploadURI (P.doSSHExport $ params)) (P.uploadURI params)
      localRepo <- prepareLocalRepo			-- Prepare the local repository for initial uploads
      cleanRoot <- P.cleanRoot cache
      cleanOS <-
              prepareEnv top
                         cleanRoot
                         buildRelease
                         (Just localRepo)
                         (P.flushRoot params)
                         (P.ifSourcesChanged params)
                         (P.includePackages params)
                         (P.excludePackages params)
                         (P.components params)
      _ <- updateCacheSources (P.ifSourcesChanged params) cleanOS
      -- Compute the essential and build essential packages, they will all
      -- be implicit build dependencies.
      globalBuildDeps <- liftIO $ buildEssential cleanOS
      -- Get a list of all sources for the local repository.
      localSources <- (\ x -> qPutStrLn "Getting local sources" >> quieter 1 x) $
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
      -- Build an apt-get environment which we can use to retrieve all the package lists
      poolOS <-prepareAptEnv top (P.ifSourcesChanged params) poolSources
      (failures, targets) <- retrieveTargetList cleanOS >>= mapM (either (return . Left) (liftIO . try . asBuildable)) >>= return . partitionEithers
      when (not $ null $ failures) (error $ intercalate "\n " $ "Some targets could not be retrieved:" : map show failures)
      buildResult <- buildTargets cache cleanOS globalBuildDeps localRepo poolOS targets
      -- If all targets succeed they may be uploaded to a remote repo
      result <- liftIO (try (upload buildResult >>= newDist)) >>=
                return . either (\ (e :: SomeException) -> Failure [show e]) id
      updateRepoCache
      return result
    where
      params = C.params cache
      baseRelease =  either (error . show) id (P.findSlice cache (P.baseRelease params))
      buildRepoSources = C.buildRepoSources cache
      buildReleaseSources = releaseSlices (P.buildRelease params) (inexactPathSlices buildRepoSources)
      buildRelease = NamedSliceList { sliceListName = SliceName (releaseName' (P.buildRelease params))
                                    , sliceList = appendSliceLists [sliceList baseRelease, buildReleaseSources] }
      doRequiredVersion :: IO ()
      doRequiredVersion =
          let abv = parseDebianVersion V.autoBuilderVersion
              rqvs = P.requiredVersion params in
          case filter (\ (v, _) -> v > abv) rqvs of
            [] -> quieter 1 $ qPutStrLn $ "Installed autobuilder version " ++ show (prettyDebianVersion abv) ++ " newer than required: " ++ show (map (first prettyDebianVersion) rqvs)
            reasons ->
                do ePutStrLn ("Installed autobuilder library version " ++ V.autoBuilderVersion ++ " is too old:")
                   mapM_ printReason reasons
                   liftIO $ exitWith (ExitFailure 1)
          where
            printReason :: (DebianVersion, Maybe String) -> IO ()
            printReason (v, s) =
                ePutStr (" Version >= " ++ show (prettyDebianVersion v) ++ " is required" ++ maybe "" ((++) ":") s)
      doShowParams = ePutStr $ "Configuration parameters:\n" ++ P.prettyPrint params
      doShowSources =
          either (error . show) doShow (P.findSlice cache (SliceName (releaseName' (P.buildRelease params))))
          where
            doShow sources =
                do qPutStrLn $ (sliceName . sliceListName $ sources) ++ ":"
                   qPutStrLn . show . pretty . sliceList $ sources
                   exitWith ExitSuccess
      doFlush =
          do top <- askTop
             qPutStrLn "Flushing cache"
             liftIO (removeRecursiveSafely top >> createDirectoryIfMissing True top)
      checkPermissions =
          do isRoot <- liftIO $ checkSuperUser
             case isRoot of
               True -> return ()
               False -> do qPutStr "You must be superuser to run the autobuilder (to use chroot environments.)"
                           liftIO $ exitWith (ExitFailure 1)
      prepareLocalRepo = (\ x -> P.localPoolDir cache >>= \ poolDir -> qPutStrLn ("Preparing local repository " ++ poolDir) >> quieter 1 x) $
          do poolDir <- P.localPoolDir cache
             let path = EnvPath (EnvRoot "") poolDir
             repo <- prepareLocalRepository path (Just Flat) >>=
                     (if P.flushPool params then flushLocalRepository else return)
             qPutStrLn $ "Preparing release main in local repository at " ++ outsidePath path
             release <- prepareRelease repo (P.buildRelease params) [] [parseSection' "main"] (P.archList params)
             case releaseRepo release of
               LocalRepo repo' ->
                   case P.cleanUp params of
                     True -> deleteGarbage repo'
                     False -> return repo'
               _ -> error "Expected local repo"
      retrieveTargetList :: MonadDeb e m => OSImage -> m [Either e Download]
      retrieveTargetList cleanOS =
          do dirtyRoot <- P.dirtyRoot cache
             qPutStr ("\n" ++ showTargets allTargets ++ "\n")
             when (P.report params) (ePutStrLn . doReport $ allTargets)
             qPutStrLn "Retrieving all source code:\n"
             countTasks' (map (retrieveTarget (chrootEnv cleanOS dirtyRoot)) (P.foldPackages (\ name spec flags l -> P.Package name spec flags : l) allTargets []))
          where
            allTargets = C.packages cache
      retrieveTarget :: MonadDeb e m => OSImage -> P.Packages -> (String, m (Either e Download))
      retrieveTarget buildOS (target :: P.Packages) =
          (show (P.spec target),
           (retrieve buildOS cache target >>= return . Right)
           `catchError` retrieveError target)
      retrieveError :: (MonadIO m, Exception e) => P.Packages -> e -> m (Either e Download)
      retrieveError target e =
          liftIO (IO.hPutStrLn IO.stderr ("Failure retrieving " ++ show (P.spec target) ++ ":\n " ++ show e)) >> return (Left e)
{-
          case (fromException e :: Maybe AsyncException) of
            -- Exit on keyboard interrupt
            Just UserInterrupt -> exitWith (ExitFailure 1)
            -- Any other exception gets reported but we continue retrieving packages
            _ -> liftIO (IO.hPutStrLn IO.stderr ("Failure retrieving " ++ show (P.spec target) ++ ":\n " ++ show e)) >> return (Left e)
-}
      upload :: (LocalRepository, [Target]) -> IO [Failing ([Output L.ByteString], NominalDiffTime)]
      upload (repo, [])
          | P.doUpload params =
              case P.uploadURI params of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> qPutStrLn "Uploading from local repository to remote" >> uploadRemote repo uri
          | True = return []
      upload (_, failed) =
          do let msg = ("Some targets failed to build:\n  " ++ intercalate "\n  " (map targetName failed))
             qPutStrLn msg
             case P.doUpload params of
               True -> qPutStrLn "Skipping upload."
               False -> return ()
             error msg
      newDist :: [Failing ([Output L.ByteString], NominalDiffTime)] -> IO (Failing ([Output L.ByteString], NominalDiffTime))
      newDist _results
          | P.doNewDist params =
              case P.uploadURI params of
                Just uri ->
                    case uriAuthority uri of
                         Just auth ->
                             let cmd = ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++
                                        " " ++ P.newDistProgram params ++ " --root " ++ uriPath uri ++
                                        (concat . map (" --create " ++) . P.createRelease $ params)) in
                             qPutStrLn "Running newdist on remote repository" >>
                             try (timeTask (runProcessF id (ShellCommand cmd) L.empty)) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success
                         Nothing ->
                             let cmd = "newdist --root " ++ uriPath uri in
                             qPutStr "Running newdist on a local repository" >>
                             try (timeTask (runProcessF id (ShellCommand cmd) L.empty)) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success
                _ -> error "Missing Upload-URI parameter"
          | True = return (Success ([], (fromInteger 0)))
      updateRepoCache :: MonadDeb e m => m ()
      updateRepoCache =
          do path <- sub "repoCache"
             live <- getApt >>= return . getRepoMap
             repoCache <- liftIO $ loadCache path
             let merged = show . map (\ (uri, x) -> (show uri, x)) . Map.toList $ Map.union live repoCache
             liftIO (removeLink path `catch` (\e -> unless (isDoesNotExistError e) (ioError e))) >> liftIO (writeFile path merged)
             return ()
          where
            isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map URI (Maybe Repository))
            loadCache path =
                do pairs <- readFile path `catch` (\ (_ :: SomeException) -> return "[]") >>= 
                            return . fromMaybe [] . maybeRead :: IO [(String, Maybe Repository)]
                   let (pairs' :: [(URI, Maybe Repository)]) =
                           catMaybes (map (\ (s, x) -> case parseURI s of
                                                         Nothing -> Nothing
                                                         Just uri -> Just (uri, x)) pairs)
                   return . Map.fromList . filter isRemote $ pairs'

-- | Perform a list of tasks with log messages.
countTasks' :: MonadIO m => [(String, m a)] -> m [a]
countTasks' tasks =
    mapM (countTask (length tasks)) (zip [1..] tasks)
    where
      countTask :: MonadIO m => Int -> (Int, (String, m a)) -> m a
      countTask count (index, (message, task)) =
          liftIO (IO.hPutStrLn IO.stderr (printf "[%2d of %2d] %s:" index count message)) >>
          task >>= \ a ->
          return a

doReport :: P.Packages -> String
doReport =
    intercalate "\n" . doReport'
    where
      doReport' :: P.Packages -> [String]
      doReport' P.NoPackage = []
      doReport' p@(P.Packages {}) = concatMap doReport' (P.packages p)
      doReport' p@(P.Package {}) =
          patched (P.spec p) ++ pinned (P.flags p)
          where
            patched :: P.RetrieveMethod -> [String]
            patched (P.Patch _ _) = [P.name p ++ " is patched"]
            patched (P.Cd _ x) = patched x
            patched (P.DataFiles x y _) = patched x ++ patched y
            patched (P.DebDir x y) = patched x ++ patched y
            patched (P.Debianize x) = patched x
            patched (P.Proc x) = patched x
            patched (P.Quilt x y) = patched x ++ patched y
            patched (P.SourceDeb x) = patched x
            patched (P.Twice x) = patched x
            patched _ = []
            pinned :: [P.PackageFlag] -> [String]
            pinned [] = []
            pinned (P.CabalPin v : more) = [P.name p ++ " is pinned at version " ++ v] ++ pinned more
            pinned (_ : more) = pinned more
