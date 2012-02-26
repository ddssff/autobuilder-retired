{-# LANGUAGE FlexibleInstances, PackageImports, ScopedTypeVariables #-}
module Debian.AutoBuilder.Params
    ( computeTopDir
    , buildCache
    , findSlice
    -- , dirtyRootOfRelease
    , cleanRootOfRelease
    , dirtyRoot
    , cleanRoot
    , localPoolDir
    , baseRelease
    , isDevelopmentRelease
    , relaxDepends
    , srcPkgName
    -- , dropSuffix
    -- , dropOneSuffix
    -- , dropAllSuffixes

    , adjustVendorTag -- Export for testing
    ) where

import Control.Exception ( SomeException, try, evaluate )
import Control.Monad.State ( get, put )
import Control.Monad.Trans ( liftIO )
import Data.List ( isSuffixOf )
import Data.Maybe ( catMaybes, fromJust )
import Data.Map ( fromList )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debian.AutoBuilder.Types.CacheRec (CacheRec(..))
import Debian.AutoBuilder.Types.PackageFlag (relaxInfo)
import Debian.AutoBuilder.Types.Packages (foldPackages)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..), TargetSpec(..))
import Debian.AutoBuilder.Types.RetrieveMethod (RetrieveMethod)
import Debian.Release ( ReleaseName(relName), releaseName' )
import Debian.Sources ( SliceName(..) )
import Debian.Repo ( EnvRoot(EnvRoot), NamedSliceList(..), parseSourcesList, verifySourcesList, repoSources )
import Debian.Repo.Monad ( AptIOT, setRepoMap )
import Debian.Repo.Types ( SliceList(..) )
import Debian.URI ( parseURI )
import qualified Debian.GenBuildDeps as G ( RelaxInfo(..), SrcPkgName(..), BinPkgName(..) )
import System.Directory ( createDirectoryIfMissing, getPermissions, writable )
import System.Environment ( getEnv )
import System.Unix.QIO (qPutStrLn)

-- import Debian.AutoBuilder.ParamClass as P ( ParamClass(..), Target, Strictness )

-- |Create a Cache object from a parameter set.
buildCache :: ParamRec -> FilePath -> AptIOT IO CacheRec
buildCache params top =
    do qPutStrLn ("Preparing autobuilder cache in " ++ top ++ "...")
       liftIO $ mapM_ (createDirectoryIfMissing True . ((top ++ "/") ++))
                  [".", "darcs", "deb-dir", "dists", "hackage", "localpools", "quilt", "tmp"]
       loadRepoCache top
       all <- mapM parseNamedSliceList (sources params)
       let uri = maybe (uploadURI params) Just (buildURI params)
       build <- maybe (return $ SliceList { slices = [] }) (repoSources Nothing) uri
       return $ CacheRec {params = params, topDir = top, allSources = all, buildRepoSources = build}
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

loadRepoCache :: FilePath -> AptIOT IO ()
loadRepoCache top =
    do qPutStrLn "Loading repo cache..."
       state <- get
       uris <- liftIO $ try (readFile (top ++ "/repoCache")) >>=
               try . evaluate . either (\ (_ :: SomeException) -> []) read >>=
               return . either (\ (_ :: SomeException) -> []) id
       put (setRepoMap (fromList (map fixURI uris)) state)
    where
      fixURI (s, x) = (fromJust (parseURI s), x)

-- Compute the top directory, try to create it, and then make sure it
-- exists.  Then we can safely return it from topDir below.
computeTopDir :: ParamRec -> IO FilePath
computeTopDir params =
    try (maybe homeDir return (topDirParam params) >>= \ top ->
         createDirectoryIfMissing True top >>
         getPermissions top >>= return . writable >>= finish top) >>=
    either (\ (e :: SomeException) -> error (show e)) return
    where
      finish _ False = error "Cache directory not writable (are you root?)"
      finish top True = return top
      homeDir = getEnv "HOME" >>= return . (++ "/.autobuilder")

-- |Find a release by name, among all the "Sources" entries given in the configuration.
findSlice :: CacheRec -> SliceName -> Either String NamedSliceList
findSlice cache dist =
    case filter ((== dist) . sliceListName) (allSources cache) of
      [x] -> Right x
      [] -> Left ("No sources.list found for " ++ sliceName dist)
      xs -> Left ("Multiple sources.lists found for " ++ sliceName dist ++ "\n" ++ show (map (sliceName . sliceListName) xs))

dirtyRootOfRelease :: CacheRec -> ReleaseName -> EnvRoot
dirtyRootOfRelease cache distro =
    EnvRoot $ topDir cache ++ "/dists/" ++ releaseName' distro ++ "/build-" ++ (show (strictness (params cache)))
    --ReleaseCache.dirtyRoot distro (show (strictness params))

cleanRootOfRelease :: CacheRec -> ReleaseName -> EnvRoot
cleanRootOfRelease cache distro =
    EnvRoot $ topDir cache ++ "/dists/" ++ releaseName' distro ++ "/clean-" ++ (show (strictness (params cache)))
    --ReleaseCache.cleanRoot distro (show (strictness params))

dirtyRoot :: CacheRec -> EnvRoot
dirtyRoot cache = dirtyRootOfRelease cache (buildRelease (params cache))
    --EnvRoot $ topDir params ++ "/dists/" ++ show (buildRelease params) ++ "/build-" ++ (show (strictness params))

cleanRoot :: CacheRec -> EnvRoot
cleanRoot cache = cleanRootOfRelease cache (buildRelease (params cache))
    -- cleanRootOfRelease params (buildRelease params)

-- |Location of the local repository for uploaded packages.
localPoolDir :: CacheRec -> FilePath
localPoolDir cache = topDir cache ++ "/localpools/" ++ releaseName' (buildRelease (params cache))

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

{-
dropAllSuffixes :: [String] -> String -> String
dropAllSuffixes suffixes s = maybe s (dropAllSuffixes suffixes) (dropOneSuffix suffixes s)
-}

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

-- | Prevent the appearance of a new binary package from
-- triggering builds of its build dependencies.  Optionally, a
-- particular source package can be specified whose rebuild will
-- be prevented.  This is used to break dependency loops, For
-- example, @Relax-Depends: ghc6 hscolour@ means \"even if ghc6
-- is rebuilt, don't rebuild hscolour even though ghc6 is one of
-- its build dependencies.\"
relaxDepends params@(ParamRec {targets = TargetSet s}) =
    makeRelaxInfo $ map (\ target -> (G.BinPkgName target, Nothing)) (globalRelaxInfo params) ++
                    foldPackages (\ spec flags xs -> xs ++ map (\ binPkg -> (G.BinPkgName binPkg, Just (G.SrcPkgName (srcPkgName spec)))) (relaxInfo flags)) [] s
relaxDepends _params = error "relaxDepends: invalid target set"

srcPkgName :: RetrieveMethod -> String
srcPkgName = error "srcPkgName"

makeRelaxInfo :: [(G.BinPkgName, Maybe G.SrcPkgName)] -> G.RelaxInfo
makeRelaxInfo xs srcPkgName binPkgName =
    Set.member binPkgName global || maybe False (Set.member binPkgName) (Map.lookup srcPkgName mp)
    where
      (global :: Set.Set G.BinPkgName, mp :: Map.Map G.SrcPkgName (Set.Set G.BinPkgName)) =
          foldr (\ entry (global', mp') ->
                     case entry of
                       (b, Just s) -> (global', Map.insertWith Set.union s (Set.singleton b) mp')
                       (b, Nothing) -> (Set.insert b global', mp')) (Set.empty, Map.empty) xs

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
