{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Debian.AutoBuilder.Tgt
    ( DL(DL)
    , BT(BT)
    -- , flags
    , relaxDepends
    , srcPkgName
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Debian.AutoBuilder.BuildTarget.Common (BuildTarget(..), Download(..))
import qualified Debian.AutoBuilder.Types.PackageFlag as P
import Debian.AutoBuilder.Types.Packages (foldPackages)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..), TargetSpec(..))
import Debian.Control (Control'(unControl), fieldValue)
import qualified Debian.GenBuildDeps as G
import Debian.Repo.SourceTree (DebianSourceTree(control'))

-- | Objects of type Tgt contain an instance of the BuildTarget type
-- class.
data DL = forall a. (Download a) => DL a
data BT = forall b. (BuildTarget b) => BT b

--getSourceTree' :: Tgt -> SourceTree
--getSourceTree' (Tgt a) = getSourceTree a

instance Download DL where
    method (DL x) = method x
    getTop (DL x) = getTop x
    revision (DL x) = revision x
    buildWrapper (DL x) action = buildWrapper x action
    logText (DL x) = logText x
    cleanTarget (DL x) = cleanTarget x
    mVersion (DL x) = mVersion x
    origTarball (DL x) = origTarball x

instance Download BT where
    method (BT x) = method x
    getTop (BT x) = getTop x
    revision (BT x) = revision x
    buildWrapper (BT x) action = buildWrapper x action
    logText (BT x) = logText x
    cleanTarget (BT x) = cleanTarget x
    mVersion (BT x) = mVersion x
    origTarball (BT x) = origTarball x

instance BuildTarget BT where
    debianSourceTree (BT x) = debianSourceTree x

-- | Prevent the appearance of a new binary package from
-- triggering builds of its build dependencies.  Optionally, a
-- particular source package can be specified whose rebuild will
-- be prevented.  This is used to break dependency loops, For
-- example, @Relax-Depends: ghc6 hscolour@ means \"even if ghc6
-- is rebuilt, don't rebuild hscolour even though ghc6 is one of
-- its build dependencies.\"
relaxDepends :: ParamRec -> BT -> G.RelaxInfo
relaxDepends params@(ParamRec {targets = TargetSet s}) tgt =
    makeRelaxInfo $ map (\ target -> (G.BinPkgName target, Nothing)) (globalRelaxInfo params) ++
                    foldPackages (\ _ _spec flags xs -> xs ++ map (\ binPkg -> (G.BinPkgName binPkg, Just (G.SrcPkgName (srcPkgName tgt)))) (P.relaxInfo flags)) [] s
relaxDepends _params _ = error "relaxDepends: invalid target set"

srcPkgName :: BT -> String
srcPkgName tgt =
    maybe (error "No Source field in control file") id (fieldValue "Source" (head (unControl (control' (debianSourceTree tgt)))))

makeRelaxInfo :: [(G.BinPkgName, Maybe G.SrcPkgName)] -> G.RelaxInfo
makeRelaxInfo xs srcPkgName binPkgName =
    Set.member binPkgName global || maybe False (Set.member binPkgName) (Map.lookup srcPkgName mp)
    where
      (global :: Set.Set G.BinPkgName, mp :: Map.Map G.SrcPkgName (Set.Set G.BinPkgName)) =
          foldr (\ entry (global', mp') ->
                     case entry of
                       (b, Just s) -> (global', Map.insertWith Set.union s (Set.singleton b) mp')
                       (b, Nothing) -> (Set.insert b global', mp')) (Set.empty, Map.empty) xs
