{-# Language ScopedTypeVariables #-}
module Debian.AutoBuilder.Types.Buildable
    ( Buildable(..)
    , asBuildable
    ) where

import Control.Exception (SomeException, try)
import Debian.AutoBuilder.Types.Download (Download(..))
import Debian.Repo.SourceTree (DebianBuildTree(debTree'), DebianSourceTree, findOneDebianBuildTree, findDebianSourceTree)
import System.IO (hPutStrLn, stderr)

-- | A replacement for the BuildTarget class and the BuildTarget.* types.  The method code
-- moves into the function that turns a RetrieveMethod into a BuildTarget.
data Buildable
    = Buildable
      { download :: Download
      , debianSourceTree :: DebianSourceTree
      -- ^ Return the debian source tree.  Every target must have
      -- this, since this program only builds debian packages.
      }

-- | Try to turn a Download into a Target.  This will throw an
-- exception if there is not a valid debian source tree at the
-- location in getTop.
asBuildable :: Download -> IO Buildable
asBuildable download =
    try (findOneDebianBuildTree (getTop download) >>=
         maybe (findDebianSourceTree (getTop download)) (return . debTree')) >>=
    either (\ (e :: SomeException) -> let msg = "asTarget " ++ show (method download) ++ " :" ++ show e in hPutStrLn stderr msg >> error msg)
           (\ tree -> return $ Buildable { download = download
                                         , debianSourceTree = tree })
