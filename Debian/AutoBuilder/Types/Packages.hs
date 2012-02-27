module Debian.AutoBuilder.Types.Packages
    ( Packages(..)
    , foldPackages
    , packageCount
    ) where

import Data.Monoid (Monoid(..))
import qualified Data.Set as Set
import Debian.AutoBuilder.Types.PackageFlag (PackageFlag)
import Debian.AutoBuilder.Types.RetrieveMethod (RetrieveMethod)

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

-- Set.fold :: (a -> b -> b) -> b -> Set a -> b

foldPackages :: (String -> RetrieveMethod -> [PackageFlag] -> r -> r) -> r -> Packages -> r
foldPackages _ r NoPackage = r
foldPackages f r x@(Package {}) = f (name x) (spec x) (flags x) r
foldPackages f r (Packages s) = Set.fold (flip (foldPackages f)) r s

packageCount :: Packages -> Int
packageCount = foldPackages (\ _ _ _ n -> n + 1) 0
