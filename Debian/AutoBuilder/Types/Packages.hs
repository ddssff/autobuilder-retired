-- | Types that specify how to obtain the source code for a package
-- and flags that describe other details of the conversion of the
-- download into a Debian source tree.
module Debian.AutoBuilder.Types.Packages
    ( Packages(..)
    , foldPackages
    , packageCount
    , RetrieveMethod(..)
    , PackageFlag(..)
    , relaxInfo
    ) where

import qualified Data.ByteString.Lazy as B
import Data.Monoid (Monoid(..))
import qualified Data.Set as Set

data Packages
    = NoPackage
    | Package
      { name :: String
      -- ^ This is only used as a reference for choosing packages to
      -- build, it is neither Debian nor Cabal package name.
      , spec :: RetrieveMethod
      , flags :: [PackageFlag]
      }
    | Packages (Set.Set Packages)
    deriving (Show, Eq, Ord)

instance Monoid Packages where
    mempty = NoPackage
    mappend NoPackage y = y
    mappend x NoPackage = x
    mappend x@(Package {}) y = mappend (Packages (Set.singleton x)) y
    mappend x y@(Package {}) = mappend x (Packages (Set.singleton y))
    mappend (Packages xs) (Packages ys) =
        let zs = Set.union xs ys in
        case Set.size zs of
          0 -> NoPackage
          1 -> Set.findMin zs
          _ -> Packages zs

foldPackages :: (String -> RetrieveMethod -> [PackageFlag] -> r -> r) -> r -> Packages -> r
foldPackages _ r NoPackage = r
foldPackages f r x@(Package {}) = f (name x) (spec x) (flags x) r
foldPackages f r (Packages s) = Set.fold (flip (foldPackages f)) r s

packageCount :: Packages -> Int
packageCount = foldPackages (\ _ _ _ n -> n + 1) 0

-- |Represents only the data resulting from parsing the spec string (which is going away)
data RetrieveMethod
    = Apt String String
    | Bzr String
    | Cd FilePath RetrieveMethod
    | Darcs String
    | DebDir RetrieveMethod RetrieveMethod
    | Debianize String
    | Dir FilePath
    | Hackage String
    | Hg String
    | Proc RetrieveMethod
    | Quilt RetrieveMethod RetrieveMethod
    | SourceDeb RetrieveMethod
    | Svn String
    | Tla String
    | Twice RetrieveMethod
    | Uri String String
    deriving (Read, Show, Eq, Ord)

-- | Flags that are applicable to any debianized package, which means
-- any package because this autobuilder only builds debs.
data PackageFlag
    = RelaxDep String
    -- ^ Build dependencies which should be ignored when deciding whether to rebuild
    | UDeb String
    -- ^ Tell the autobuilder that a binary package name is a udeb.  This means that
    -- we can ignore the package when we are deciding whether we need to do an arch
    -- only build.
    | Patch B.ByteString
    -- ^ Apply the patch (currently only works on targets downloaded
    -- use RetrieveMethod Debianize.)
    | Maintainer String
    -- ^ Use the given string as maintainer name and email
    | OmitLTDeps
    -- ^ Ignore the << (less than) part of a version dependency when
    -- converting cabal wildcard dependencies.  These can lead to
    -- complex relationships that can't be translated into debian
    -- dependency.
    | AptPin String
    -- ^ Specify the exact debian version of a package to retrieve via apt-get
    | CabalPin String
    -- ^ Specify the exact version of the Cabal package to download from Hackage.
    | ExtraDep String
    -- ^ Build dependencies which should be added to the
    -- debian/control file via the --build-dep flag of cabal-debian.
    | ExtraDevDep String
    -- ^ Install dependencies which should be added to the Depends
    -- entry for the dev package in the debian/control file via the
    -- --dev-dep flag of cabal-debian
    | MapDep String String
    -- ^ Tell cabal-debian to map the first argument (a name that
    -- appears in Extra-Libraries field of the cabal file) to the
    -- second argument (a debian binary package name) using the
    -- --map-dep flag of cabal-debian.
    | DebVersion String
    -- ^ The exact debian version number to insert into the changelog.
    -- An exception will be thrown if the version in the retrieved
    -- package looks newer than this.  This causes the --deb-version
    -- flag to be passed to cabal-debian.
    | Revision String
    -- ^ Pass --revision <string> to cabal-debian so a suffix will be
    -- added to the cabal version to get the debian version.  By
    -- default this is -1~hackage1.  Debian policy says this should
    -- either be empty or begin with a dash.
    | Epoch String Int
    -- ^ Set the epoch number in the debian version number generated
    -- for the given cabal package
    | DarcsTag String
    -- ^ When doing a darcs get pass this string to darcs via the --tag flag.
    deriving (Show, Eq, Ord)

relaxInfo :: [PackageFlag] -> [String]
relaxInfo flags =
    foldr f [] flags
    where f (RelaxDep s) ss = s : ss
          f _ ss = ss
