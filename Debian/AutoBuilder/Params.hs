{-# LANGUAGE FlexibleInstances, PackageImports, ScopedTypeVariables #-}
module Debian.AutoBuilder.Params
    ( computeTopDir
    , buildCache
    , findSlice
    , cleanRootOfRelease
    , dirtyRoot
    , cleanRoot
    , localPoolDir
    , baseRelease
    , isDevelopmentRelease
    , adjustVendorTag -- Export for testing
    ) where

import Control.Exception ( SomeException, try, evaluate )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.List ( isSuffixOf )
import Data.Maybe ( catMaybes, fromJust, fromMaybe )
import Data.Map ( fromList )
import Debian.AutoBuilder.Types.CacheRec (CacheRec(..))
import Debian.AutoBuilder.Types.Packages (Packages)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Release ( ReleaseName(relName), releaseName' )
import Debian.Sources ( SliceName(..) )
import Debian.Repo ( EnvRoot(EnvRoot), NamedSliceList(..), parseSourcesList, verifySourcesList, repoSources )
import Debian.Repo.Monads.AptState (setRepoMap)
import Debian.Repo.Monads.MonadApt (MonadApt(getApt, putApt))
import Debian.Repo.Monads.MonadDeb (MonadDeb)
import Debian.Repo.Monads.Top (MonadTop, askTop, sub)
import Debian.Repo.Types ( SliceList(..) )
import Debian.URI ( parseURI )
import System.Directory ( createDirectoryIfMissing, getPermissions, writable )
import System.Environment ( getEnv )
import System.FilePath ((</>))
import System.Process.Progress (qPutStrLn)

-- |Create a Cache object from a parameter set.
buildCache :: MonadDeb e m => ParamRec -> Packages -> m CacheRec
buildCache params packages =
    do top <- askTop
       qPutStrLn ("Preparing autobuilder cache in " ++ top ++ "...")
       mapM_ (\ dir -> sub dir >>= liftIO . createDirectoryIfMissing True)
                  [".", "darcs", "deb-dir", "dists", "hackage", "localpools", "quilt", "tmp"]
       loadRepoCache
       all <- mapM parseNamedSliceList (sources params)
       let uri = maybe (uploadURI params) Just (buildURI params)
       build <- maybe (return $ SliceList { slices = [] }) (repoSources Nothing) uri
       return $ CacheRec {params = params, allSources = all, buildRepoSources = build, packages = packages}
    where
      parseNamedSliceList (name, text) = 
          do sources <- (verifySourcesList Nothing . parseSourcesList) text
             return $ NamedSliceList { sliceListName = SliceName name, sliceList = sources }

-- |An instance of RunClass contains all the information we need to
-- run the autobuilder.
-- class (ParamClass a, CacheClass a) => RunClass a

-- |Make a ('ParamClass', 'CacheClass') pair an instance ParamClass,
-- CacheClass, and RunClass.
-- instance (ParamClass p) => RunClass (p, Cache)

loadRepoCache :: MonadDeb e m => m ()
loadRepoCache =
    do top <- askTop
       qPutStrLn "Loading repo cache..."
       state <- getApt
       uris <- liftIO $ try (readFile (top ++ "/repoCache")) >>=
               try . evaluate . either (\ (_ :: SomeException) -> []) read >>=
               return . either (\ (_ :: SomeException) -> []) id
       putApt (setRepoMap (fromList (map fixURI uris)) state)
    where
      fixURI (s, x) = (fromJust (parseURI s), x)

-- Compute the top directory, try to create it, and then make sure it
-- exists.  Then we can safely return it from topDir below.
computeTopDir :: ParamRec -> IO FilePath
computeTopDir params =
    do home <- getEnv "HOME"
       let top = fromMaybe (home ++ "/.autobuilder") (topDirParam params)
       createDirectoryIfMissing True top
       canWrite <- getPermissions top >>= return . writable
       case canWrite of
         False -> error "Cache directory not writable (are you root?)"
         True -> return top

-- |Find a release by name, among all the "Sources" entries given in the configuration.
findSlice :: CacheRec -> SliceName -> Either String NamedSliceList
findSlice cache dist =
    case filter ((== dist) . sliceListName) (allSources cache) of
      [x] -> Right x
      [] -> Left ("No sources.list found for " ++ sliceName dist)
      xs -> Left ("Multiple sources.lists found for " ++ sliceName dist ++ "\n" ++ show (map (sliceName . sliceListName) xs))

dirtyRootOfRelease :: MonadTop m => CacheRec -> ReleaseName -> m EnvRoot
dirtyRootOfRelease cache distro =
    askTop >>= \ top ->
    return $ EnvRoot $ top </> "dists" </> releaseName' distro </> "build-" ++ (show (strictness (params cache)))
    --ReleaseCache.dirtyRoot distro (show (strictness params))

cleanRootOfRelease :: MonadTop m => CacheRec -> ReleaseName -> m EnvRoot
cleanRootOfRelease cache distro =
    askTop >>= \ top ->
    return $ EnvRoot $ top ++ "/dists/" ++ releaseName' distro ++ "/clean-" ++ (show (strictness (params cache)))

dirtyRoot :: MonadTop m => CacheRec -> m EnvRoot
dirtyRoot cache = dirtyRootOfRelease cache (buildRelease (params cache))

cleanRoot :: MonadTop m => CacheRec -> m EnvRoot
cleanRoot cache = cleanRootOfRelease cache (buildRelease (params cache))

-- |Location of the local repository for uploaded packages.
localPoolDir :: MonadTop m => CacheRec -> m FilePath
localPoolDir cache =
    askTop >>= \ top ->
    return $ top </> "localpools" </> releaseName' (buildRelease (params cache))

-- | Packages uploaded to the build release will be compatible
-- with packages in this release.
baseRelease :: ParamRec -> SliceName
baseRelease params =
    maybe (error $ "Unknown release suffix: " ++ rel) SliceName
              (dropOneSuffix (releaseSuffixes params) rel)
    where rel = (relName (buildRelease params))

dropSuffix suffix x = take (length x - length suffix) x

dropSuffixMaybe :: String -> String -> Maybe String
dropSuffixMaybe suffix x = if isSuffixOf suffix x then Just (dropSuffix suffix x) else Nothing

dropOneSuffix suffixes s =
    case catMaybes (map (`dropSuffixMaybe` s) suffixes) of
      [s'] -> Just s'
      _ -> Nothing

-- | Signifies that the release we are building for is a development
-- (or unstable) release.  This means we the tag we add doesn't need
-- to include @~<release>@, since there are no newer releases to
-- worry about trumping.
isDevelopmentRelease params =
    elem (topReleaseName (relName (buildRelease params))) (developmentReleaseNames params)
    where
      topReleaseName name =
          foldr dropSuff name (releaseSuffixes params)
          where dropSuff suff name = if isSuffixOf suff name then dropSuffix suff name else name

-- |Adjust the vendor tag so we don't get trumped by Debian's new +b
-- notion for binary uploads.  The version number of the uploaded
-- binary packages may have "+bNNN" appended, which would cause
-- them to trump the versions constructed by the autobuilder.  So, we
-- prepend a "+" to the vendor string if there isn't one, and if the
-- vendor string starts with the character b or something less, two
-- plus signs are prepended.
adjustVendorTag s =
    newprefix ++ suffix
    where (_oldprefix, suffix) = span (== '+') s
          newprefix = if suffix < "b" then "++" else "+" 
