{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
module Debian.AutoBuilder.Main 
    ( main
    ) where

import Control.Applicative.Error (Failing(..))
import Control.Exception(SomeException, IOException, try)
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
import qualified Debian.AutoBuilder.ParamClass as P
import Debian.AutoBuilder.ParamRec()    -- Instances only
import Debian.AutoBuilder.Target(Target, targetName, buildTargets, readSpec, showTargets)
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
                         LocalRepository(LocalRepository), outsidePath,)
import Debian.URI(URIAuth(uriUserInfo, uriRegName), URI(uriScheme, uriPath, uriAuthority), parseURI)
import Debian.Version(DebianVersion, parseDebianVersion)
import System.Unix.Process(Output)
import Extra.List(consperse)
import Extra.Lock(withLock)
import Extra.Misc(checkSuperUser)
import System.Directory(createDirectoryIfMissing)
import System.Posix.Files(removeLink)
import System.Exit(ExitCode(..), exitWith)
import qualified System.IO as IO
import System.IO.Error(isDoesNotExistError)
import System.Unix.Directory(removeRecursiveSafely)
import System.Unix.Progress (modQuietness, ePutStr, ePutStrLn, timeTask, lazyCommandF, quieter, qPutStrLn, qPutStr)

-- | Called from the configuration script, this processes a list of
-- parameter sets.
main :: P.ParamClass p => [p] -> IO ()
main [] = error $ "No parameter sets"
main paramSets =
    runAptIO (mapM doParameterSet paramSets) >>=
    checkResults >>
    IO.hFlush IO.stderr

-- |Process one set of parameters.  Usually there is only one, but there
-- can be several which are run sequentially.
doParameterSet :: P.ParamClass p => p -> AptIOT IO (Either IOException (Either SomeException (Failing ([Output], NominalDiffTime))))
doParameterSet params =
    modQuietness (const (- (P.verbosity params))) $
    P.buildCache params >>= \ cache ->
    let params' = (params, cache) in
    withLock (P.topDir params' ++ "/lockfile") (tryAB . runParameterSet $ params')

runParameterSet :: P.RunClass p => p -> AptIOT IO (Failing ([Output], NominalDiffTime))
runParameterSet params =
    do
      qPutStrLn $ "topDir=" ++ show (P.topDir params)
      -- liftIO $ writeParams params
      lift doRequiredVersion
      lift doShowParams
      doShowSources
      doFlush
      checkPermissions
      maybe (return ()) (verifyUploadURI (P.doSSHExport $ params)) (P.uploadURI params)
      localRepo <- prepareLocalRepo			-- Prepare the local repository for initial uploads
      qPutStrLn "Preparing clean build environment"
      cleanOS <- (prepareEnv
                         (P.topDir params)
                         (P.cleanRoot params)
                         buildRelease
                         (Just localRepo)
                         (P.flushRoot params)
                         (P.ifSourcesChanged params)
                         (P.includePackages params ++ ["haskell-debian-utils"])
                         (P.excludePackages params)
                         (P.components params))
      qPutStrLn "Updating cache sources"
      _ <- updateCacheSources (P.ifSourcesChanged params) cleanOS

      -- Compute the essential and build essential packages, they will all
      -- be implicit build dependencies.
      qPutStrLn "Computing build essentials"
      globalBuildDeps <- liftIO $ buildEssential cleanOS
      -- Get a list of all sources for the local repository.
      qPutStrLn "Getting local sources"
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
      quieter 1 (qPutStrLn ("poolSources:\n" ++ show (sliceList poolSources)))
      -- Build an apt-get environment which we can use to retrieve all the package lists
      poolOS <- iStyle $ prepareAptEnv (P.topDir params) (P.ifSourcesChanged params) poolSources
      targets <- prepareTargetList 	-- Make a the list of the targets we hope to build
      case partitionEithers targets of
        ([], targets') ->
            do buildResult <- buildTargets params cleanOS globalBuildDeps localRepo poolOS targets'
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
               buildResult <- buildTargets params cleanOS globalBuildDeps localRepo poolOS ok
               -- If all targets succeed they may be uploaded to a remote repo
               uploadResult <- upload buildResult
               -- This processes the remote incoming dir
               result <- lift (newDist uploadResult)
               updateRepoCache params
               return result
        (bad, _) ->
            do lift (qPutStrLn ("Could not prepare source code of some targets:\n " ++ intercalate "\n " (map (intercalate "\n  ") bad)))
               return (Failure ("Could not prepare source code of some targets:" : map (intercalate "\n  ") bad))
-}
    where
      baseRelease =  either (error . show) id (P.findSlice params (P.baseRelease params))
      buildRepoSources = P.buildRepoSources params
      buildReleaseSources = releaseSlices (P.buildRelease params) (inexactPathSlices buildRepoSources)
      buildRelease = NamedSliceList { sliceListName = SliceName (releaseName' (P.buildRelease params))
                                    , sliceList = appendSliceLists [sliceList baseRelease, buildReleaseSources] }
      doRequiredVersion :: IO ()
      doRequiredVersion =
          case filter (\ (v, _) -> v > parseDebianVersion V.autoBuilderVersion) (P.requiredVersion params) of
            [] -> return ()
            reasons ->
                do qPutStrLn ("Installed autobuilder library version " ++ V.autoBuilderVersion ++ " is too old:")
                   mapM_ printReason reasons
                   liftIO $ exitWith (ExitFailure 1)                    
          where
            printReason :: (DebianVersion, Maybe String) -> IO ()
            printReason (v, s) =
                qPutStr (" Version >= " ++ show v ++ " is required" ++ maybe "" ((++) ":") s)
      doShowParams = when (P.showParams params) (qPutStr $ "Configuration parameters:\n" ++ P.prettyPrint params)
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
               False -> do qPutStr "You must be superuser to run the autobuilder (to use chroot environments.)"
                           liftIO $ exitWith (ExitFailure 1)
      prepareLocalRepo =
          do let path = EnvPath (EnvRoot "") (P.localPoolDir params)
             repo <- prepareLocalRepository path (Just Flat) >>=
                     (if P.flushPool params then flushLocalRepository else return)
             ePutStrLn $ "Preparing release main in local repository at " ++ outsidePath path
             release <- prepareRelease repo (P.buildRelease params) [] [parseSection' "main"] (P.archList params)
             case releaseRepo release of
               LocalRepo repo' ->
                   case P.cleanUp params of
                     True -> deleteGarbage repo'
                     False -> return repo'
      prepareTargetList =
          do ePutStr (showTargets allTargets)
             ePutStrLn "Retrieving all source code:\n"
             countTasks (map (\ target -> (P.sourcePackageName target, tryAB (quieter 3 (readSpec params (P.sourceSpec target))))) allTargets)
          where
            allTargets = filter (\ x -> not (elem (P.sourcePackageName x) (P.omitTargets params))) (Set.toList (P.targets params))
            --listDiff a b = Set.toList (Set.difference (Set.fromList a) (Set.fromList b))
      upload :: (LocalRepository, [Target]) -> AptIOT IO [Failing ([Output], NominalDiffTime)]
      upload (repo, [])
          | P.doUpload params =
              case P.uploadURI params of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> ePutStr "Uploading from local repository" >> uploadRemote repo uri
          | True = return []
      upload (_, failed) =
          do
            ePutStr ("Some targets failed to build:\n  " ++ consperse "\n  " (map targetName failed) ++ "\n")
            case P.doUpload params of
              True -> ePutStr "Skipping upload."
              False -> return ()
            liftIO $ exitWith (ExitFailure 1)
      newDist :: [Failing ([Output], NominalDiffTime)] -> IO (Failing ([Output], NominalDiffTime))
      newDist results
          | P.doNewDist params =
              case P.uploadURI params of
                Just uri ->
                    do ePutStrLn ("Upload results:\n  " ++ intercalate "\n  " (map show results))
                       case uriAuthority uri of
                         Just auth ->
                             let cmd = ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++
                                        " " ++ P.newDistProgram params ++ " --root " ++ uriPath uri ++
                                        (concat . map (" --create " ++) . P.createRelease $ params)) in
                             ePutStr "Running newdist on remote repository" >>
                             try (timeTask (lazyCommandF cmd L.empty)) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success
                         Nothing ->
                             let cmd = "newdist --root " ++ uriPath uri in
                             ePutStr "Running newdist on a local repository" >>
                             try (timeTask (lazyCommandF cmd L.empty)) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success
                _ -> error "Missing Upload-URI parameter"
          | True = return (Success ([], (fromInteger 0)))
      iStyle = id {- setStyle (addPrefixes " " " ") -}
      --top = P.topDir params
      --dryRun = P.dryRun params
      -- flush = P.flushSource params
      updateRepoCache :: AptIOT IO ()
      updateRepoCache =
          do let path = P.topDir params  ++ "/repoCache"
             live <- get >>= return . getRepoMap
             cache <- liftIO $ loadCache path
             let merged = show . map (\ (uri, x) -> (show uri, x)) . Map.toList $ Map.union live cache
             liftIO (removeLink path `Prelude.catch` (\e -> unless (isDoesNotExistError e) (ioError e))) >> liftIO (writeFile path merged)
             return ()
          where
            isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map URI (Maybe Repository))
            loadCache path =
                do pairs <- try (readFile path >>= return . read) >>=
                            either (\ (e :: SomeException) -> ePutStrLn ("Couldn't load cache: " ++ show e) >> return []) return
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
checkResults [Right (Left e)] = (ePutStrLn (show e)) >> liftIO (exitWith $ ExitFailure 1)
checkResults [Right (Right _)] = (liftIO $ exitWith ExitSuccess)
checkResults [Left e] = ePutStrLn ("Failed to obtain lock: " ++ show e ++ "\nAbort.") >> liftIO (exitWith (ExitFailure 1))
checkResults list =
    do mapM_ (\ (num, result) -> ePutStrLn ("Parameter set " ++ show num ++ ": " ++ showResult result)) (zip [(1 :: Int)..] list)
       case filter isLeft list of
         [] -> liftIO (exitWith ExitSuccess)
         _ -> liftIO (exitWith (ExitFailure 1))
    where showResult (Right (Left e)) = show e
          showResult (Right (Right _)) = "Ok"
          showResult (Left e) = "Ok (" ++ show e ++ ")"
          isLeft (Right (Left _)) = True
          isLeft (Left _) = True
          isLeft (Right (Right _)) = False    
