module Debian.AutoBuilder.Types.PackageFlag
    ( PackageFlag(..)
    , CabalVersion(..)
    , relaxInfo
    ) where

import qualified Data.ByteString.Lazy as B

data PackageFlag
    = RelaxDep String		-- ^ Build dependencies which should be ignored when deciding whether to rebuild
    | ExtraDep String		-- ^ Build dependencies which should be added to the debian/control file
    | ExtraDevDep String	-- ^ Install dependencies which should be added to the Depends entry for the dev package in the debian/control file
    | MapDep String String	-- ^ Tell cabal-debian to map the first argument (a name that appears in Extra-Libraries field of the cabal file) to the second argument (a debian binary package name.)
    | DebVersion String         -- ^ The exact debian version number to insert into the changelog.  An exception will be thrown if the hackage version looks newer than this.
    | Revision String           -- ^ Pass --revision <string> to cabal-debian so a suffix will be added to the cabal version to get the debian version.  By default this is -1~hackage1.  Debian policy says this should either be empty or begin with a dash.
    | Epoch String Int          -- ^ Set the epoch number in the version number of the given cabal package
    | Patch B.ByteString        -- ^ Apply the patch
    | Maintainer String         -- ^ Use the given string as maintainer name and email
    | OmitLTDeps                -- ^ Don't add the << part when converting cabal wildcard dependencies
    | CabalVersion CabalVersion
    deriving (Show, Eq, Ord)

data CabalVersion
    = Pin String
    | Newest
    | NewerThan String
    deriving (Show, Eq, Ord)

relaxInfo :: [PackageFlag] -> [String]
relaxInfo flags =
    foldr f [] flags
    where f (RelaxDep s) ss = s : ss
          f _ ss = ss
