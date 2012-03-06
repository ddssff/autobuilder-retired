module Debian.AutoBuilder.Types.PackageFlag
    ( PackageFlag(..)
    , AptFlag(..)
    , CabalFlag(..)
    , DarcsFlag(..)
    -- , CabalVersion(..)
    , relaxInfo
    ) where

import qualified Data.ByteString.Lazy as B

data AptFlag
    = AptPin String             -- ^ Specify the exact debian version of a package to retrieve via apt-get
    deriving (Read, Show, Eq, Ord)

data DarcsFlag
    = DarcsTag String           -- ^ When doing a darcs get pass this string to darcs via the --tag flag.
    deriving (Read, Show, Eq, Ord)

data CabalFlag
    = CabalPin String
    | ExtraDep String		-- ^ Build dependencies which should be added to the debian/control file via the --build-dep flag of cabal-debian.
    | ExtraDevDep String	-- ^ Install dependencies which should be added to the Depends entry for the dev package in the
                                -- debian/control file via the --dev-dep flag of cabal-debian
    | MapDep String String	-- ^ Tell cabal-debian to map the first argument (a name that appears in Extra-Libraries field of
                                -- the cabal file) to the second argument (a debian binary package name) using the --map-dep flag of cabal-debian.
    | DebVersion String         -- ^ The exact debian version number to insert into the changelog.  An exception will be thrown if
                                -- the hackage version looks newer than this.  This causes the --deb-version flag to be passed to cabal-debian.
    | Revision String           -- ^ Pass --revision <string> to cabal-debian so a suffix will be added to the cabal version to get
                                -- the debian version.  By default this is -1~hackage1.  Debian policy says this should either be
                                -- empty or begin with a dash.
    | Epoch String Int          -- ^ Set the epoch number in the version number of the given cabal package
    deriving (Read, Show, Eq, Ord)

-- | Flags that are applicable to any debianized package, which means
-- any package because this autobuilder only builds debs.
data PackageFlag
    = RelaxDep String		-- ^ Build dependencies which should be ignored when deciding whether to rebuild
    | Patch B.ByteString        -- ^ Apply the patch
    | Maintainer String         -- ^ Use the given string as maintainer name and email
    | OmitLTDeps                -- ^ Don't add the << part when converting cabal wildcard dependencies
    | AptFlag AptFlag
    | CabalFlag CabalFlag
    | DarcsFlag DarcsFlag
    deriving (Show, Eq, Ord)

{-
data CabalVersion
    = Pin String
    | Newest
    | NewerThan String
    deriving (Read, Show, Eq, Ord)
-}

relaxInfo :: [PackageFlag] -> [String]
relaxInfo flags =
    foldr f [] flags
    where f (RelaxDep s) ss = s : ss
          f _ ss = ss
