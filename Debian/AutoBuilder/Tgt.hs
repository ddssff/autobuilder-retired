{-# LANGUAGE ExistentialQuantification #-}
module Debian.AutoBuilder.Tgt
    ( Tgt(Tgt)
    , prepareDir
    ) where

import Debian.Repo
import Debian.AutoBuilder.BuildTarget (BuildTarget, Dir(Dir))
import Debian.AutoBuilder.ParamClass (ParamClass)

-- | Objects of type Tgt contain an instance of the BuildTarget type
-- class.
data Tgt = forall a. (Show a, BuildTarget a) => Tgt a

instance Show Tgt where
    show (Tgt a) = show a

--getSourceTree' :: Tgt -> SourceTree
--getSourceTree' (Tgt a) = getSourceTree a

-- |Prepare a Dir target
prepareDir :: (ParamClass p) => p -> FilePath -> IO Tgt
prepareDir _params path = findSourceTree path >>= return . Tgt . Dir
