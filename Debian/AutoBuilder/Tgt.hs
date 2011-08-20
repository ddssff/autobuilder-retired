{-# LANGUAGE ExistentialQuantification #-}
module Debian.AutoBuilder.Tgt
    ( Tgt(Tgt)
    ) where

import Debian.AutoBuilder.BuildTarget.Common (BuildTarget(..))

-- | Objects of type Tgt contain an instance of the BuildTarget type
-- class.
data Tgt = forall a. (Show a, BuildTarget a) => Tgt a

instance Show Tgt where
    show (Tgt a) = show a

--getSourceTree' :: Tgt -> SourceTree
--getSourceTree' (Tgt a) = getSourceTree a

instance BuildTarget Tgt where
    getTop params (Tgt x) = getTop params x
    cleanTarget params (Tgt x) path = cleanTarget params x path
    revision params (Tgt x) = revision params x
    buildWrapper params os tree status (Tgt x) action = buildWrapper params os tree status x action
    logText (Tgt x) result = logText x result
    mVersion (Tgt x) = mVersion x
