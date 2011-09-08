{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
module Debian.AutoBuilder.Main 
    ( main
    ) where

import Control.Applicative.Error (Failing(..))
import Control.Exception(SomeException, IOException, try, catch)
import Control.Monad.State(MonadIO(..), MonadTrans(..), MonadState(get))
import Control.Monad(when, unless)
import qualified Data.ByteString.Lazy as L
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time(NominalDiffTime)
import Data.List(intercalate)
import Data.Maybe(catMaybes)
--import qualified Debian.AutoBuilder.OldParams as O
import Debian.AutoBuilder.BuildTarget (readSpec)
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Target(Target, targetName, buildTargets, showTargets)
import qualified Debian.AutoBuilder.Version as V
import Debian.Release (parseSection', releaseName')
import Debian.Sources (SliceName(..))
import Debian.Repo.AptImage(prepareAptEnv)
import Debian.Repo.Cache(updateCacheSources)
import Debian.Repo.Insert(deleteGarbage)
import Debian.Repo.Monad (AptIOT, getRepoMap, runAptIO, tryAB, countTasks)
import Debian.Repo.LocalRepository(prepareLocalRepository, flushLocalRepository)
import Debian.Repo.OSImage(buildEssential, prepareEnv)
import Debian.Repo.Release(prepareRelease)
import Debian.Repo.Repository(uploadRemote, verifyUploadURI)
import Debian.Repo.Slice(appendSliceLists, inexactPathSlices, releaseSlices, repoSources)
import Debian.Repo.Types(EnvRoot(EnvRoot), EnvPath(..),
                         Layout(Flat), Release(releaseRepo),
                         NamedSliceList(..), Repository(LocalRepo),
                         LocalRepository(LocalRepository), outsidePath, q12)
import Debian.URI(URIAuth(uriUserInfo, uriRegName), URI(uriScheme, uriPath, uriAuthority), parseURI)
import Debian.Version(DebianVersion, parseDebianVersion)
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
import System.Unix.QIO (quieter, quieter', qPutStrLn, qPutStr)

-- | Called from the configuration script, this processes a list of
-- parameter sets.
main :: [P.ParamRec] -> IO ()
main [] = error $ "No parameter sets"
main paramSets =
    runAptIO (mapM doParameterSet paramSets) >>=
    checkResults >>
    IO.hFlush IO.stderr

-- |Process one set of parameters.  Usually there is only one, but there
-- can be several which are run sequentially.
doParameterSet :: P.ParamRec -> AptIOT IO (Either IOException (Either SomeException (Failing ([Output], NominalDiffTime))))
doParameterSet params =
    quieter (const (- (P.verbosity params))) $
    quieter (+ 2) (P.buildCache params) >>= \ cache ->
    withLock (P.topDir cache ++ "/lockfile") (tryAB . runParameterSet $ cache)

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
      maybe (return ()) (verifyUploadURI (P.doSSHExport $ (P.params cache))) (P.uploadURI (P.params cache))
      localRepo <- prepareLocalRepo			-- Prepare the local repository for initial uploads
      cleanOS <-
              prepareEnv (P.topDir cache)
                         (P.cleanRoot cache)
                         buildRelease
                         (Just localRepo)
                         (P.flushRoot (P.params cache))
                         (P.ifSourcesChanged (P.params cache))
                         (P.includePackages (P.params cache) ++ ["haskell-debian-utils"])
                         (P.excludePackages (P.params cache))
                         (P.components (P.params cache))
      _ <- updateCacheSources (P.ifSourcesChanged (P.params cache)) cleanOS
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
      poolOS <-prepareAptEnv (P.topDir cache) (P.ifSourcesChanged (P.params cache)) poolSources
      targets <- prepareTargetList 	-- Make a the list of the targets we hope to build
      case partitionEithers targets of
        ([], targets') ->
            do buildResult <- buildTargets cache cleanOS globalBuildDeps localRepo poolOS targets'
               -- If all targets succeed they may be uploaded to a remote repo
               result <- tryAB (upload buildResult >>= lift . newDist) >>=
                         return . either (\ e -> Failure [show e]) id
               updateRepoCache
               return result
        (failures, _) ->
            error $ "Some targets could not be prepared:\n " ++ intercalate "\n " (map show failures)
{-
      case partitionFailing targets of
        ([], ok) ->
            do -- Build all the targets
               buildResult <- buildTargets cache cleanOS globalBuildDeps localRepo poolOS ok
               -- If all targets succeed they may be uploaded to a remote repo
               uploadResult <- upload buildResult
               -- This processes the remote incoming dir
               result <- lift (newDist uploadResult)
               updateRepoCache cache
               return result
        (bad, _) ->
            do lift (qPutStrLn ("Could not prepare source code of some targets:\n " ++ intercalate "\n " (map (intercalate "\n  ") bad)))
               return (Failure ("Could not prepare source code of some targets:" : map (intercalate "\n  ") bad))
-}
    where
      baseRelease =  either (error . show) id (P.findSlice cache (P.baseRelease (P.params cache)))
      buildRepoSources = P.buildRepoSources cache
      buildReleaseSources = releaseSlices (P.buildRelease (P.params cache)) (inexactPathSlices buildRepoSources)
      buildRelease = NamedSliceList { sliceListName = SliceName (releaseName' (P.buildRelease (P.params cache)))
                                    , sliceList = appendSliceLists [sliceList baseRelease, buildReleaseSources] }
      doRequiredVersion :: IO ()
      doRequiredVersion =
          let abv = parseDebianVersion V.autoBuilderVersion
              rqvs = P.requiredVersion (P.params cache) in
          case filter (\ (v, _) -> v > abv) rqvs of
            [] -> quieter' (+ 1) $ qPutStrLn $ "Installed autobuilder version " ++ show abv ++ " newer than required: " ++ show rqvs
            reasons -> quieter (const 0) $
                do qPutStrLn ("Installed autobuilder library version " ++ V.autoBuilderVersion ++ " is too old:")
                   mapM_ printReason reasons
                   liftIO $ exitWith (ExitFailure 1)                    
          where
            printReason :: (DebianVersion, Maybe String) -> IO ()
            printReason (v, s) =
                qPutStr (" Version >= " ++ show v ++ " is required" ++ maybe "" ((++) ":") s)
      doShowParams = when (P.showParams (P.params cache)) $
                       quieter (const 0) (qPutStr $ "Configuration parameters:\n" ++ P.prettyPrint (P.params cache))
      doShowSources =
          if (P.showSources (P.params cache)) then
              either (error . show) doShow (P.findSlice cache (SliceName (releaseName' (P.buildRelease (P.params cache))))) else
              return ()
          where
            doShow sources = quieter (const 0) $
                do qPutStrLn $ (sliceName . sliceListName $ sources) ++ ":"
                   qPutStrLn . show . sliceList $ sources
                   liftIO $ exitWith ExitSuccess
      -- FIXME: This may be too late
      doFlush
          | P.flushAll (P.params cache) =
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
                     (if P.flushPool (P.params cache) then flushLocalRepository else return)
             qPutStrLn $ "Preparing release main in local repository at " ++ outsidePath path
             release <- prepareRelease repo (P.buildRelease (P.params cache)) [] [parseSection' "main"] (P.archList (P.params cache))
             case releaseRepo release of
               LocalRepo repo' ->
                   case P.cleanUp (P.params cache) of
                     True -> deleteGarbage repo'
                     False -> return repo'
               _ -> error "Expected local repo"
      prepareTargetList =
          do qPutStr ("\n" ++ showTargets allTargets)
             qPutStrLn "Retrieving all source code:\n"
             countTasks (map (\ target -> (P.name target, tryAB (readSpec cache (P.flags target) (P.spec target)))) allTargets)
          where
            allTargets = Set.toList targetSet
            targetSet = case P.targets (P.params cache) of
                          P.TargetSet s -> s
                          _ -> error "prepareTargetList: invalid target set"

      upload :: (LocalRepository, [Target]) -> AptIOT IO [Failing ([Output], NominalDiffTime)]
      upload (repo, [])
          | P.doUpload (P.params cache) =
              case P.uploadURI (P.params cache) of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> qPutStrLn "Uploading from local repository to remote" >> uploadRemote repo uri
          | True = return []
      upload (_, failed) =
          do
            qPutStrLn ("Some targets failed to build:\n  " ++ intercalate "\n  " (map targetName failed))
            case P.doUpload (P.params cache) of
              True -> qPutStrLn "Skipping upload."
              False -> return ()
            liftIO $ exitWith (ExitFailure 1)
      newDist :: [Failing ([Output], NominalDiffTime)] -> IO (Failing ([Output], NominalDiffTime))
      newDist _results
          | P.doNewDist (P.params cache) =
              case P.uploadURI (P.params cache) of
                Just uri ->
                    case uriAuthority uri of
                         Just auth ->
                             let cmd = ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++
                                        " " ++ P.newDistProgram (P.params cache) ++ " --root " ++ uriPath uri ++
                                        (concat . map (" --create " ++) . P.createRelease $ (P.params cache))) in
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

-- |The result of processing a set of parameters is either an
-- exception or a completion code, or, if we fail to get a lock,
-- nothing.  For a single result we can print a simple message,
-- for multiple paramter sets we need to print a summary.
checkResults :: [Either IOException (Either SomeException (Failing ([Output], NominalDiffTime)))] -> IO ()
checkResults [Right (Left e)] = (qPutStrLn (show e)) >> liftIO (exitWith $ ExitFailure 1)
checkResults [Right (Right _)] = (liftIO $ exitWith ExitSuccess)
checkResults [Left e] = qPutStrLn ("Failed to obtain lock: " ++ show e ++ "\nAbort.") >> liftIO (exitWith (ExitFailure 1))
checkResults list =
    do mapM_ (\ (num, result) -> qPutStrLn ("Parameter set " ++ show num ++ ": " ++ showResult result)) (zip [(1 :: Int)..] list)
       case partitionEithers list of
         ([], _) -> liftIO (exitWith ExitSuccess)
         (_, _) -> liftIO (exitWith (ExitFailure 1))
    where showResult (Right (Left e)) = show e
          showResult (Right (Right _)) = "Ok"
          showResult (Left e) = "Ok (" ++ show e ++ ")"
