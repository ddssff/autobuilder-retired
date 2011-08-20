{-# LANGUAGE ExistentialQuantification #-}
module Debian.AutoBuilder.Tgt
    ( Tgt(Tgt)
    ) where

import Debian.AutoBuilder.BuildTarget.Common (BuildTarget)

-- | Objects of type Tgt contain an instance of the BuildTarget type
-- class.
data Tgt = forall a. (Show a, BuildTarget a) => Tgt a

instance Show Tgt where
    show (Tgt a) = show a

--getSourceTree' :: Tgt -> SourceTree
--getSourceTree' (Tgt a) = getSourceTree a
