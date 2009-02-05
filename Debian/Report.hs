module Debian.Report where

import Debian.Apt.Index (Fetcher, Compression(..), update, controlFromIndex')
import Debian.Control.ByteString
import Debian.Repo.Types
import Debian.Version

import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Extra.HaXml (mkTxt)
import Text.XML.HaXml

-- * General Package Map Builders

-- |create a map of (package name, extracted field) from a list of index files
--
-- NOTE: we could merge all the files into a single control and then
-- run packageMap over that. We currently do it one control file at a
-- time to avoid having all the control files loaded in memory at
-- once. However, I am not sure that property is actually occuring
-- anyway. So, this should be revisited.
makePackageMap :: (Paragraph -> a) -> (a -> a -> a) -> [(FilePath, Compression)] -> IO (M.Map B.ByteString a)
makePackageMap _ _ [] = return M.empty
makePackageMap extractValue resolveConflict ((path, compression):is) =
    do r <- controlFromIndex' compression path
       case r of
         (Left e) -> error (show e)
         (Right c) ->
             do let pm = packageMap extractValue resolveConflict c
                pms <- makePackageMap extractValue resolveConflict is
                return $ M.unionWith resolveConflict pm pms

-- |create a map of (package name, max version) from a single control file
packageMap :: (Paragraph -> a) -> (a -> a -> a) -> Control -> M.Map B.ByteString a
packageMap extractValue resolveConflict control =
    M.fromListWith resolveConflict (map packageTuple (unControl control))
    where
      packageTuple paragraph = (fromJust $ fieldValue "Package" paragraph, extractValue paragraph)

-- |extract the version number from a control paragraph
extractVersion :: Paragraph -> Maybe DebianVersion
extractVersion paragraph = fmap (parseDebianVersion . B.unpack)  $ fieldValue "Version" paragraph

-- * Trump Report

-- |compare two sources.list and find all the packages in the second that trump packages in the first
-- see also: |trumpedMap|
trumped :: Fetcher -- ^ function for downloading package indexes
        -> FilePath -- ^ cache directory to store index files in (must already exist)
        -> String -- ^ binary architecture 
        -> [DebSource] -- ^ sources.list a
        -> [DebSource] -- ^ sources.list b
        -> IO (M.Map B.ByteString (DebianVersion, DebianVersion)) -- ^ a map of trumped package names to (version a, version b)
trumped fetcher cacheDir arch sourcesA sourcesB =
    do indexesA <- update fetcher cacheDir arch (filter isDebSrc sourcesA)
       pmA <- makePackageMap (fromJust . extractVersion) max (map fromJust indexesA)
       indexesB <- update fetcher cacheDir arch (filter isDebSrc sourcesB)
       pmB <- makePackageMap (fromJust . extractVersion) max (map fromJust indexesB)
       return (trumpedMap pmA pmB)
    where
      isDebSrc ds = sourceType ds == DebSrc

-- |calculate all the trumped packages
trumpedMap :: M.Map B.ByteString DebianVersion -- ^ package map a
           -> M.Map B.ByteString DebianVersion -- ^ package map b
           -> M.Map B.ByteString (DebianVersion, DebianVersion) -- ^ trumped packages (version a, version b)
trumpedMap pmA pmB =
    M.foldWithKey (checkTrumped pmB) M.empty pmA
    where
      checkTrumped pm package aVersion trumpedPM =
          case M.lookup package pm of
            (Just bVersion)
              | bVersion > aVersion -> M.insert package (aVersion, bVersion) trumpedPM
            _ -> trumpedPM

-- |create <trumped /> XML element and children from a trumped Map
trumpedXML :: M.Map B.ByteString (DebianVersion, DebianVersion) -> CFilter
trumpedXML trumpedMap' =
    mkElem "trumped" (map mkTrumpedPackage (M.toAscList trumpedMap' ))
    where
      mkTrumpedPackage (package, (oldVersion, newVersion)) =
          mkElem "trumpedPackage"
                     [ mkElem "package" [ mkTxt (B.unpack package) ]
                     , mkElem "oldVersion" [ mkTxt (show oldVersion) ]
                     , mkElem "newVersion" [ mkTxt (show newVersion) ]
                     ]
