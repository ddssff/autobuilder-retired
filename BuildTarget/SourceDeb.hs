-- |A SourceDeb target modifies another target to provide an unpacked debian source tree
-- when a debian source package is found.  A debian source package is a @.dsc@ file, a
-- @.tar.gz@ file, and an optional @.diff.gz@ file.
module BuildTarget.SourceDeb where

import BuildTarget
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Control.Monad
import System.Directory
--import Debian.IO
import Debian.TIO
import Debian.Types
import System.Unix.Process
import qualified Debian.Control.String as S
import qualified Debian.Version as V

-- | Treat the data returned by a target as a source deb.
data SourceDeb = SourceDeb Tgt EnvPath String

instance Show SourceDeb where
    show (SourceDeb t _ _) = "sourcedeb:" ++ show t

documentation = [ "sourcedeb:<target> - A target of this form unpacks the source deb"
                , "retrieved by the original target and presents an unpacked source"
                , "tree for building.  Thus, the original target should retrieve a"
                , "directory containing a .dsc file, a .tar.gz, and an optional"
                , ".diff.gz file." ]

-- |SourceDeb targets inherit the revision string of the target they modify.
instance BuildTarget SourceDeb where
    getTop (SourceDeb (Tgt _) dir _) = dir
    revision (SourceDeb (Tgt t) _ _) =
        BuildTarget.revision t >>= return . either Left (Right . ("sourcedeb:" ++))
    logText (SourceDeb (Tgt t) _ _) revision = logText t revision ++ " (source deb)"

-- |Given the BuildTarget for the base target, prepare a SourceDeb BuildTarget
-- by unpacking the source deb.
prepareSourceDeb :: Tgt -> TIO (Either String Tgt)
prepareSourceDeb (Tgt base) =
    do let top = getTop base
       dscFiles <- lift (getDirectoryContents (outsidePath top)) >>=
                   return . filter (isSuffixOf ".dsc")
       dscInfo <- mapM (\ name -> lift (readFile (outsidePath top ++ "/" ++ name) >>= return . S.parseControl name)) dscFiles
       case sortBy compareVersions (zip dscFiles dscInfo) of
         [] -> return . Left $ "Invalid sourcedeb base: no .dsc file in " ++ show base
         (dscName, Right (S.Control (dscInfo : _))) : _ ->
             do out <- lift (lazyCommand (unpack top dscName) [])
                case exitCodeOnly out of
                  [ExitSuccess] -> return $ makeTarget top dscInfo dscName
                  _ -> return . Left $ ("*** FAILURE: " ++ unpack top dscName)
         (dscName, _) : _ -> return . Left $ "Invalid .dsc file: " ++ dscName
    where
      makeTarget top dscInfo dscName =
          case (S.fieldValue "Source" dscInfo, maybe Nothing (Just . V.parseDebianVersion)
                     (S.fieldValue "Version" dscInfo)) of
            (Just package, Just version) ->
                Right . Tgt $ (SourceDeb (Tgt base) top (package ++ "-" ++ V.version version))
            _ -> Left $ "Invalid .dsc file: " ++ dscName
      unpack top dscName = "cd " ++ outsidePath top ++ " && dpkg-source -x " ++ dscName
      compareVersions (name2, info2) (name1, info1) =
          case (info1, info2) of
            (Right (S.Control (para1 : _)), Right (S.Control (para2 : _))) ->
                compare (maybe Nothing (Just . V.parseDebianVersion) (S.fieldValue "Version" para1))
                        (maybe Nothing (Just . V.parseDebianVersion) (S.fieldValue "Version" para2))
            _  -> error $ "Invalid .dsc file: " ++ name1 ++ " or " ++ name2
