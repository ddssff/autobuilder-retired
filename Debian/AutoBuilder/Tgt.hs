{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Debian.AutoBuilder.Tgt
    ( Tgt(Tgt, Top)
    , flags
    ) where

import Debian.AutoBuilder.BuildTarget.Common (BuildTarget(..))
import qualified Debian.AutoBuilder.Types.PackageFlag as P

-- | Objects of type Tgt contain an instance of the BuildTarget type
-- class.
data Tgt
    = forall a. (Show a, BuildTarget a) => Tgt a
    | forall a. (Show a, BuildTarget a) => Top [P.PackageFlag] a

instance Show Tgt where
    show (Tgt a) = show a
    show (Top _ a) = show a

--getSourceTree' :: Tgt -> SourceTree
--getSourceTree' (Tgt a) = getSourceTree a

instance BuildTarget Tgt where
    getTop params (Tgt x) = getTop params x
    getTop params (Top _ x) = getTop params x
    cleanTarget params (Tgt x) path = cleanTarget params x path
    cleanTarget params (Top _ x) path = cleanTarget params x path
    revision params (Tgt x) = revision params x
    revision params (Top _ x) = revision params x
    buildWrapper params os tree status (Tgt x) action = buildWrapper params os tree status x action
    buildWrapper params os tree status (Top _ x) action = buildWrapper params os tree status x action
    logText (Tgt x) result = logText x result
    logText (Top _ x) result = logText x result
    mVersion (Tgt x) = mVersion x
    mVersion (Top _ x) = mVersion x
    origTarball cache (Tgt x) = origTarball cache x
    origTarball cache (Top _ x) = origTarball cache x

flags :: Tgt -> [P.PackageFlag]
flags (Top fs _) = fs
flags (Tgt _) = []