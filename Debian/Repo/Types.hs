module Debian.Repo.Types 
    ( EnvRoot(..)
    , EnvPath(..)
    , outsidePath
    , appendPath
    , rootEnvPath
    -- * Repository
    , PackageVersion(..)
    , PkgVersion(..)
    , Repo(..)
    , libraryCompatibilityLevel
    , compatibilityFile
    , Repository(..)
    , LocalRepository(..)
    , Layout(..)
    -- * Release
    , ReleaseName(..)
    , parseReleaseName
    , ReleaseInfo(..)
    , Arch(..)
    , archName
    , Section(..)
    , SubSection(..)
    , sectionNameOfSubSection
    , sectionName
    , sectionName'
    , parseSection
    , parseSection'
    , Release(..)
    , releaseName'
    , releaseName
    , releaseComponents
    , releaseArchitectures
    -- * Each line of the sources.list represents a slice of a repository
    , SourceType(..)
    , DebSource(..)
    , SliceName(..)
    , Slice(..)
    , SliceList(..)
    , NamedSliceList(..)
    -- * Package, Source and Binary Debs
    , PackageIndex(..)
    , PackageIndexLocal
    , PackageID(..)
    , BinaryPackage(..)
    , SourcePackage(..)
    , SourceFileSpec(..)
    , PackageIDLocal
    , BinaryPackageLocal
    , SourcePackageLocal
    -- * Cached OS Image
    , AptCache(..)
    , AptBuildCache(..)
    , AptImage(..)
    ) where

import qualified Debian.Control.ByteString as B
import qualified Debian.Relation as B
import Debian.URI
import Debian.Version

import Control.Exception (throw)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char
import Data.List
import Data.Maybe
import Network.URI
import System.FilePath ((</>))
import System.Posix.Types
--import System.Unix.Process

-- |The root directory of an OS image.
data EnvRoot = EnvRoot { rootPath :: FilePath } deriving (Ord, Eq, Read, Show)

-- |A directory inside of an OS image.
data EnvPath = EnvPath { envRoot :: EnvRoot
                       , envPath :: FilePath
                       } deriving (Ord, Eq, Read, Show)

outsidePath :: EnvPath -> FilePath
outsidePath path = rootPath (envRoot path) ++ envPath path

appendPath :: FilePath -> EnvPath -> EnvPath
appendPath suff path = path { envPath = envPath path ++ suff }

rootEnvPath :: FilePath -> EnvPath
rootEnvPath s = EnvPath { envRoot = EnvRoot "", envPath = s }

--------------------- REPOSITORY -----------------------

-- | The Repository type reprents any instance of the Repo class, so
-- it might be local or remote.
--data Repository = forall a. (Repo a) => Repository a
data Repository
    = LocalRepo LocalRepository
    | VerifiedRepo URIString [ReleaseInfo]
    | UnverifiedRepo URIString
    deriving (Read, Show)

instance Ord Repository where
    compare a b = compare (repoURI a) (repoURI b)

instance Eq Repository where
    a == b = compare a b == EQ

data LocalRepository
    = LocalRepository
      { repoRoot :: EnvPath
      , repoLayout :: (Maybe Layout)
      , repoReleaseInfoLocal :: [ReleaseInfo]
      } deriving (Read, Show, Ord, Eq)

-- |The possible file arrangements for a repository.  An empty
-- repository does not yet have either of these attributes.
data Layout = Flat | Pool deriving (Eq, Ord, Read, Show)

instance Repo Repository where
    repoURI (LocalRepo (LocalRepository path _ _)) = fromJust . parseURI $ "file://" ++ envPath path
    repoURI (VerifiedRepo uri _) = fromJust (parseURI uri)
    repoURI (UnverifiedRepo uri) = fromJust (parseURI uri)
{-
    repoURI (Repository a) = repoURI a
    repositoryCompatibilityLevel (Repository a) = repositoryCompatibilityLevel a
    repoReleaseInfo (Repository a) = repoReleaseInfo a
-}
    repoReleaseInfo (LocalRepo (LocalRepository _ _ info)) = info
    repoReleaseInfo (VerifiedRepo _ info) = info
    repoReleaseInfo (UnverifiedRepo _uri) = error "No release info for unverified repository"

instance Repo LocalRepository where
    repoURI (LocalRepository path _ _) = fromJust . parseURI $ "file://" ++ envPath path
    repoReleaseInfo (LocalRepository _ _ info) = info

class (Ord t, Eq t) => Repo t where
    repoURI :: t -> URI
    repositoryCompatibilityLevel :: t -> IO (Maybe Int)
    repositoryCompatibilityLevel r =
        fileFromURI uri' >>= either throw (return . parse . L.unpack)
        where
          uri' = uri {uriPath = uriPath uri </> compatibilityFile}
          uri = repoURI r
{-
        lazyCommand cmd L.empty >>= return . collectOutput >>= test cmd
        where
          cmd = "curl -s -g '" ++ uriToString id (repoURI r) "" </> compatibilityFile ++ "'"
          test _ (out, _err, [ExitSuccess]) = return (parse (L.unpack out))
          test cmd (_, _, _) = error $ "*** FAILURE: " ++ cmd
-}
          parse :: String -> Maybe Int
          parse text = case takeWhile isDigit text of
                         "" -> Nothing
                         s -> Just . read $ s
    -- | This method returns a list of all the release in the
    -- repository.  This can be used to identify all of the files
    -- in the repository that are not garbage.
    repoReleaseInfo :: t -> [ReleaseInfo]
    checkCompatibility :: t -> IO ()
    checkCompatibility repo =
        do level <- repositoryCompatibilityLevel repo
           case level of
             Nothing -> return ()
             Just n | n >= libraryCompatibilityLevel -> return ()
             Just n -> error ("Compatibility error: repository level " ++ show n ++
                              " < library level " ++ show libraryCompatibilityLevel ++ ", please upgrade.")

-- |The name of the file which holds the repository's compatibility
-- level.
compatibilityFile :: FilePath
compatibilityFile = "repository-compat"

-- | The compatibility level of this library and any applications
-- which use it.  It is an error if we try to use a repository whose
-- compatibility level is higher than this, a newer version of the
-- library must be used.  This value was increased from 1 to 2 due
-- to a new version number tagging policy.
libraryCompatibilityLevel :: Int
libraryCompatibilityLevel = 2

class (Eq a, Ord a) => PackageVersion a where
    pkgName :: a -> B.PkgName
    pkgVersion :: a -> DebianVersion

-- |This is an old type which is still used to interface with the
-- Debian.Relation module.
data PkgVersion = PkgVersion { getName :: B.PkgName
                             , getVersion :: DebianVersion
                             } deriving (Eq, Ord)

instance PackageVersion PkgVersion where
    pkgName = getName
    pkgVersion = getVersion

instance Show PkgVersion where
    show v = getName v ++ "=" ++ show (getVersion v)

-------------------- RELEASE --------------------

-- |A distribution (aka release) name.  This type is expected to refer
-- to a subdirectory of the dists directory which is at the top level
-- of a repository.
data ReleaseName = ReleaseName { relName :: String } deriving (Eq, Ord, Read, Show)

parseReleaseName :: String -> ReleaseName
parseReleaseName name = ReleaseName {relName = unEscapeString name}

releaseName' :: ReleaseName -> String
releaseName' (ReleaseName {relName = s}) = escapeURIString isAllowedInURI s

-- FIXME: The lists here should be sets so that == and compare work properly.
data ReleaseInfo = ReleaseInfo { releaseInfoName :: ReleaseName
                               , releaseInfoAliases :: [ReleaseName]
                               , releaseInfoArchitectures :: [Arch]
                               , releaseInfoComponents :: [Section]
                               } deriving (Eq, Ord, Read, Show)

-- |The types of architecture that a package can have, either Source
-- or some type of binary architecture.
data Arch = Source | Binary String deriving (Read, Show, Eq, Ord)

archName :: Arch -> String
archName Source = "source"
archName (Binary arch) = arch

-- |A section of a repository such as main, contrib, non-free,
-- restricted.  The indexes for a section are located below the
-- distribution directory.
newtype Section = Section String deriving (Read, Show, Eq, Ord)

-- |A package's subsection is only evident in its control information,
-- packages from different subsections all reside in the same index.
data SubSection = SubSection { section :: Section, subSectionName :: String } deriving (Read, {-Show,-} Eq, Ord)

sectionName :: SubSection -> String
sectionName (SubSection (Section "main") y) = y
sectionName (SubSection x y) = sectionName' x ++ "/" ++ y

sectionName' :: Section -> String
sectionName' (Section s) = escapeURIString isAllowedInURI s

sectionNameOfSubSection :: SubSection -> String
sectionNameOfSubSection = sectionName' . section

-- |Parse the value that appears in the @Section@ field of a .changes file.
-- (Does this need to be unesacped?)
parseSection section =
    case span (/= '/') section of
      (x, "") -> SubSection (Section "main") x
      ("main", y) -> SubSection (Section "main") y
      (x, y) -> SubSection (Section x) (tail y)

parseSection' name =
    Section (unEscapeString name)

data Release = Release { releaseRepo :: Repository
                       , releaseInfo :: ReleaseInfo
                       } deriving (Eq, Ord, Show)

releaseName :: Release -> ReleaseName
releaseName = releaseInfoName . releaseInfo
--releaseAliases :: Release -> [ReleaseName]
--releaseAliases = releaseInfoAliases . releaseInfo
releaseComponents :: Release -> [Section]
releaseComponents = releaseInfoComponents . releaseInfo
releaseArchitectures :: Release -> [Arch]
releaseArchitectures = releaseInfoArchitectures . releaseInfo

----------------- SLICES (SOURCES.LIST ENTRIES) ---------------

data SourceType
    = Deb | DebSrc
    deriving (Eq, Ord)

data DebSource
    = DebSource
    { sourceType :: SourceType
    , sourceUri :: URI
    , sourceDist :: Either String (ReleaseName, [Section])
    } deriving (Eq, Ord)

instance Show SourceType where
    show Deb = "deb"
    show DebSrc = "deb-src"

instance Show DebSource where
    show (DebSource thetype theuri thedist) =
        (show thetype) ++ " "++ uriToString id theuri " " ++
        (case thedist of
           Left exactPath -> escape exactPath
           Right (dist, sections) ->
               releaseName' dist ++ " " ++ intercalate " " (map sectionName' sections))
        where escape = escapeURIString isAllowedInURI

-- |This is a name given to a combination of parts of one or more
-- releases that can be specified by a sources.list file.
data SliceName = SliceName { sliceName :: String } deriving (Eq, Ord)

data Slice
    = Slice { sliceRepo :: Repository
            , sliceSource :: DebSource
            } deriving (Eq, Ord)

data SliceList = SliceList {slices :: [Slice]} deriving (Eq, Ord)

data NamedSliceList
    = NamedSliceList { sliceList :: SliceList
                     , sliceListName :: SliceName
                     } deriving (Eq, Ord)

instance Show Slice where
    show = show . sliceSource

instance Show SliceList where
    show = concat . map ((++ "\n") . show) . slices

---------------- PACKAGES AND PACKAGE INDEXES -------------

-- |The PackageIndex type represents a file containing control
-- information about debian packages, either source or binary.
-- Though the control information for a binary package does not
-- specify an architecture, the architecture here is that of
-- the environment where the package information is cached.
data PackageIndex
    = PackageIndex { packageIndexRelease :: Release
                   , packageIndexComponent :: Section
                   , packageIndexArch :: Arch
                   } deriving (Eq, Ord, Show)

type PackageIndexLocal = PackageIndex

instance Show BinaryPackage where
    show p = packageName (packageID p) ++ "-" ++ show (packageVersion (packageID p))

-- | The 'PackageID' type fully identifies a package by name, version,
-- and a 'PackageIndex' which identifies the package's release,
-- component and architecture.
data PackageID
    = PackageID
      { packageIndex :: PackageIndex
      , packageName :: String
      , packageVersion :: DebianVersion
      } deriving (Eq, Ord, Show)

-- | The 'BinaryPackage' type adds to the 'PackageID' type the control
-- information obtained from the package index.
data BinaryPackage
    = BinaryPackage
      { packageID :: PackageID
      , packageInfo :: B.Paragraph
      , pDepends :: B.Relations
      , pPreDepends :: B.Relations
      , pConflicts ::B.Relations
      , pReplaces :: B.Relations
      , pProvides :: B.Relations
      }

instance Ord BinaryPackage where
    compare a b = compare (packageID a) (packageID b)

instance Eq BinaryPackage where
    a == b = (packageID a) == (packageID b)

data SourcePackage
    = SourcePackage
      { sourcePackageID :: PackageID
      , sourceParagraph :: B.Paragraph
      , sourceDirectory :: String
      , sourcePackageFiles :: [SourceFileSpec]
      }

data SourceFileSpec
    = SourceFileSpec
      { sourceFileMD5sum :: String
      , sourceFileSize :: FileOffset
      , sourceFileName :: FilePath
      }

type PackageIDLocal = PackageID
type BinaryPackageLocal = BinaryPackage
type SourcePackageLocal = SourcePackage

---------------------- CACHED OS IMAGE ---------------------

class (Ord t, Eq t, Show t) => AptCache t where
    globalCacheDir :: t -> FilePath
    -- | The directory you might chroot to.
    rootDir :: t -> EnvRoot
    -- | The sources.list without the local repository
    aptBaseSliceList :: t -> SliceList
    -- | The build architecture
    aptArch :: t -> Arch
    -- | Return the all source packages in this AptCache.
    aptSourcePackages :: t -> [SourcePackage]
    -- | Return the all binary packages for the architecture of this AptCache.
    aptBinaryPackages :: t -> [BinaryPackage]
    -- | Name of release
    aptReleaseName :: t -> ReleaseName

class AptCache t => AptBuildCache t where
    -- | The sources.list
    aptSliceList :: t -> SliceList

data AptImage =
    AptImage { aptGlobalCacheDir :: FilePath
             , aptImageRoot :: EnvRoot
             , aptImageArch :: Arch
             , aptImageSliceList :: SliceList
             , aptImageReleaseName :: ReleaseName
             , aptImageSourcePackages :: [SourcePackage]
             , aptImageBinaryPackages :: [BinaryPackage]
             }
