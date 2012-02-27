{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
module Debian.AutoBuilder.Main 
    ( main
    ) where

import Control.Arrow (first)
import Control.Applicative.Error (Failing(..))
import Control.Exception(Exception, SomeException, try, catch)
import Control.Monad(foldM, when, unless)
import Control.Monad.State(MonadIO(..), MonadTrans(..), MonadState(get), runStateT)
import qualified Data.ByteString.Lazy as L
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Time(NominalDiffTime)
import Data.List(intercalate)
import Data.Maybe(catMaybes)
--import qualified Debian.AutoBuilder.OldParams as O
import Debian.AutoBuilder.BuildTarget (retrieve)
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Target(buildTargets, showTargets)
import Debian.AutoBuilder.TargetType (Target, targetName)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Version as V
import Debian.Release (parseSection', releaseName')
import Debian.Sources (SliceName(..))
import Debian.Repo.AptImage(prepareAptEnv)
import Debian.Repo.Cache(updateCacheSources)
import Debian.Repo.Insert(deleteGarbage)
import Debian.Repo.Monad (AptIOT, AptState, initState, getRepoMap, tryAB)
import Debian.Repo.LocalRepository(prepareLocalRepository, flushLocalRepository)
import Debian.Repo.OSImage(buildEssential, prepareEnv)
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
import System.Unix.Directory(removeRecursiveSafely)
import System.Unix.Process(Output)
import System.Unix.Progress (timeTask, lazyCommandF)
import System.Unix.QIO (quieter, quieter', qPutStrLn, qPutStr, q12)
import Text.Printf ( printf )

-- | Called from the configuration script, this processes a list of
-- parameter sets.
main :: [P.ParamRec] -> IO ()
main [] = error $ "No parameter sets"
main paramSets =
    foldM (\ (xs, s) paramSet ->
               try (doParameterSet s paramSet) >>=
               return . either (\ (e :: SomeException) -> (Left e : xs, initState)) (\ (result, s') -> (Right result : xs, s')))
          ([], initState)
          paramSets >>=
    checkResults . fst >>
    IO.hFlush IO.stderr

-- |The result of processing a set of parameters is either an
-- exception or a completion code, or, if we fail to get a lock,
-- nothing.  For a single result we can print a simple message,
-- for multiple paramter sets we need to print a summary.
checkResults :: Exception e => [Either e (Failing ([Output], NominalDiffTime))] -> IO ()
checkResults [Left e] = qPutStrLn (show e ++ "\nAbort.") >> liftIO (exitWith (ExitFailure 1))
checkResults [Right _] = return ()
checkResults list =
    case partitionEithers list of
      ([], []) -> return ()
      (_, _) -> error $ intercalate "\n  " (map (\ (num, result) -> "Parameter set " ++ show num ++ ": " ++ showResult result) (zip [(1 :: Int)..] list))
    where showResult (Left e) = show e
          showResult (Right _) = "Ok"

-- |Process one set of parameters.  Usually there is only one, but there
-- can be several which are run sequentially.
doParameterSet :: AptState -> P.ParamRec -> IO (Failing ([Output], NominalDiffTime), AptState)
doParameterSet state params =
    quieter (const (- (P.verbosity params))) $
    do top <- P.computeTopDir params
       withLock (top ++ "/lockfile") (runStateT (quieter (+ 2) (P.buildCache params top) >>= runParameterSet) state)

runParameterSet :: P.CacheRec -> AptIOT IO (Failing ([Output], NominalDiffTime))
runParameterSet cache =
    do
      -- qPutStrLn $ "topDir=" ++ show (P.topDir cache)
      -- liftIO $ writeParams cache
      lift doRequiredVersion
      lift doShowParams
      doShowSources
      doFlush
      checkPermissions
      maybe (return ()) (verifyUploadURI (P.doSSHExport $ params)) (P.uploadURI params)
      localRepo <- prepareLocalRepo			-- Prepare the local repository for initial uploads
      cleanOS <-
              prepareEnv top
                         (P.cleanRoot cache)
                         buildRelease
                         (Just localRepo)
                         (P.flushRoot params)
                         (P.ifSourcesChanged params)
                         (P.includePackages params {- ++ ["haskell-debian-utils"] -})
                         (P.excludePackages params)
                         (P.components params)
      _ <- updateCacheSources (P.ifSourcesChanged params) cleanOS
      -- Compute the essential and build essential packages, they will all
      -- be implicit build dependencies.
      globalBuildDeps <- liftIO $ buildEssential cleanOS
      -- Get a list of all sources for the local repository.
      localSources <- q12 "Getting local sources" $
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
      poolOS <-prepareAptEnv (P.topDir cache) (P.ifSourcesChanged params) poolSources
      targets <- retrieveTargetList
      let (failures, targets') = partitionEithers targets
      when (not (null failures))
           (do let msg = intercalate "\n " ("Some targets could not be retrieved:" : map show failures)
               liftIO $ IO.hPutStrLn IO.stderr msg
               error msg)
      buildResult <- buildTargets cache cleanOS globalBuildDeps localRepo poolOS targets'
      -- If all targets succeed they may be uploaded to a remote repo
      result <- tryAB (upload buildResult >>= lift . newDist) >>=
                return . either (\ e -> Failure [show e]) id
      updateRepoCache
      return result
    where
      top = P.topDir cache
      params = P.params cache
      baseRelease =  either (error . show) id (P.findSlice cache (P.baseRelease params))
      buildRepoSources = P.buildRepoSources cache
      buildReleaseSources = releaseSlices (P.buildRelease params) (inexactPathSlices buildRepoSources)
      buildRelease = NamedSliceList { sliceListName = SliceName (releaseName' (P.buildRelease params))
                                    , sliceList = appendSliceLists [sliceList baseRelease, buildReleaseSources] }
      doRequiredVersion :: IO ()
      doRequiredVersion =
          let abv = parseDebianVersion V.autoBuilderVersion
              rqvs = P.requiredVersion params in
          case filter (\ (v, _) -> v > abv) rqvs of
            [] -> quieter' (+ 1) $ qPutStrLn $ "Installed autobuilder version " ++ show (prettyDebianVersion abv) ++ " newer than required: " ++ show (map (first prettyDebianVersion) rqvs)
            reasons -> quieter (const 0) $
                do qPutStrLn ("Installed autobuilder library version " ++ V.autoBuilderVersion ++ " is too old:")
                   mapM_ printReason reasons
                   liftIO $ exitWith (ExitFailure 1)                    
          where
            printReason :: (DebianVersion, Maybe String) -> IO ()
            printReason (v, s) =
                qPutStr (" Version >= " ++ show (prettyDebianVersion v) ++ " is required" ++ maybe "" ((++) ":") s)
      doShowParams = when (P.showParams params) $
                       quieter (const 0) (qPutStr $ "Configuration parameters:\n" ++ P.prettyPrint params)
      doShowSources =
          if (P.showSources params) then
              either (error . show) doShow (P.findSlice cache (SliceName (releaseName' (P.buildRelease params)))) else
              return ()
          where
            doShow sources = quieter (const 0) $
                do qPutStrLn $ (sliceName . sliceListName $ sources) ++ ":"
                   qPutStrLn . show . sliceList $ sources
                   liftIO $ exitWith ExitSuccess
      -- FIXME: This may be too late
      doFlush
          | P.flushAll params =
              do qPutStrLn "Flushing cache"
                 liftIO $ removeRecursiveSafely (P.topDir cache)
                 liftIO $ createDirectoryIfMissing True (P.topDir cache)
          | True = return ()
      checkPermissions =
          do isRoot <- liftIO $ checkSuperUser
             case isRoot of
               True -> return ()
               False -> do qPutStr "You must be superuser to run the autobuilder (to use chroot environments.)"
                           liftIO $ exitWith (ExitFailure 1)
      prepareLocalRepo = q12 ("Preparing local repository " ++ P.localPoolDir cache) $
          do let path = EnvPath (EnvRoot "") (P.localPoolDir cache)
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
      -- retrieveTargetList :: AptIOT IO (Either SomeException Tgt)
      retrieveTargetList =
          do qPutStr ("\n" ++ showTargets allTargets ++ "\n")
             qPutStrLn "Retrieving all source code:\n"
             countTasks' (map (\ (target :: P.Packages) ->
                                   (show (P.spec target),
                                    tryAB (retrieve cache (P.flags target) (P.spec target)) >>=
                                    either (\ e -> liftIO (IO.hPutStrLn IO.stderr ("Failure retrieving " ++ show (P.spec target) ++ ":\n " ++ show e)) >>
                                                   return (Left e))
                                           (return . Right)))
                              (P.foldPackages (\ name spec flags l -> P.Package name spec flags : l) [] allTargets))
          where
{-          allTargets = P.foldPackages (\ name spec flags l -> 
                                             P.Package name spec flags : l) [] (case P.targets params of
                                                                                  P.TargetSet s -> s
                                                                                  _ -> error "relaxDepends: invalid target set") -}
            allTargets = case P.targets params of
                           P.TargetSet s -> s
                           _ -> error "retrieveTargetList: invalid target set"
      upload :: (LocalRepository, [Target]) -> AptIOT IO [Failing ([Output], NominalDiffTime)]
      upload (repo, [])
          | P.doUpload params =
              case P.uploadURI params of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> qPutStrLn "Uploading from local repository to remote" >> uploadRemote repo uri
          | True = return []
      upload (_, failed) =
          do
            qPutStrLn ("Some targets failed to build:\n  " ++ intercalate "\n  " (map targetName failed))
            case P.doUpload params of
              True -> qPutStrLn "Skipping upload."
              False -> return ()
            liftIO $ exitWith (ExitFailure 1)
      newDist :: [Failing ([Output], NominalDiffTime)] -> IO (Failing ([Output], NominalDiffTime))
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
                             try (timeTask (lazyCommandF cmd L.empty)) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success
                         Nothing ->
                             let cmd = "newdist --root " ++ uriPath uri in
                             qPutStr "Running newdist on a local repository" >>
                             try (timeTask (lazyCommandF cmd L.empty)) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success
                _ -> error "Missing Upload-URI parameter"
          | True = return (Success ([], (fromInteger 0)))
      updateRepoCache :: AptIOT IO ()
      updateRepoCache =
          do let path = P.topDir cache  ++ "/repoCache"
             live <- get >>= return . getRepoMap
             repoCache <- liftIO $ loadCache path
             let merged = show . map (\ (uri, x) -> (show uri, x)) . Map.toList $ Map.union live repoCache
             liftIO (removeLink path `catch` (\e -> unless (isDoesNotExistError e) (ioError e))) >> liftIO (writeFile path merged)
             return ()
          where
            isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map URI (Maybe Repository))
            loadCache path =
                do pairs <- try (readFile path >>= return . read) >>=
                            either (\ (e :: SomeException) -> qPutStrLn ("Couldn't load cache: " ++ show e) >> return []) return
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
