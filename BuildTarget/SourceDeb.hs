-- |A SourceDeb target modifies another target to provide an unpacked debian source tree
-- when a debian source package is found.  A debian source package is a @.dsc@ file, a
-- @.tar.gz@ file, and an optional @.diff.gz@ file.
module BuildTarget.SourceDeb where

import BuildTarget
import Data.List
import Data.Maybe
import Control.Monad
import System.Directory
import Debian.IO
import Debian.Types
import qualified Debian.Control.String as S
import qualified Debian.Version as V

-- | Treat the data returned by a target as a source deb.
data SourceDeb = SourceDeb Tgt EnvPath String

instance Show SourceDeb where
    show (SourceDeb t _ _) = "sourcedeb:" ++ show t

-- |SourceDeb targets inherit the revision string of the target they modify.
instance BuildTarget SourceDeb where
    getTop (SourceDeb (Tgt _) dir _) = dir
    revision (SourceDeb (Tgt t) _ _) =
        BuildTarget.revision t >>= return . maybe Nothing (Just . ("sourcedeb:" ++))
    logText (SourceDeb (Tgt t) _ _) revision = logText t revision ++ " (source deb)"

-- |Given the BuildTarget for the base target, prepare a SourceDeb BuildTarget
-- by unpacking the source deb.
prepareSourceDeb :: Tgt -> AptIO Tgt
prepareSourceDeb (Tgt base) =
    do let top = getTop base
       dscFiles <- io (getDirectoryContents (outsidePath top)) >>=
                   return . filter (isSuffixOf ".dsc")
       dscInfo <- mapM (\ name -> io (readFile (outsidePath top ++ "/" ++ name) >>= return . S.parseControl name)) dscFiles
       case sortBy compareVersions (zip dscFiles dscInfo) of
         [] -> error $ "Invalid sourcedeb base: no .dsc file in " ++ show base
         (dscName, Right (S.Control (dscInfo : _))) : _ ->
             do systemTask $ "cd " ++ outsidePath top ++ " && dpkg-source -x " ++ dscName
                case (S.fieldValue "Source" dscInfo, maybe Nothing (Just . V.parseDebianVersion)
                           (S.fieldValue "Version" dscInfo)) of
                  (Just package, Just version) ->
                      return . Tgt $ (SourceDeb (Tgt base) top (package ++ "-" ++ V.version version))
                  _ -> error $ "Invalid .dsc file: " ++ dscName
         (dscName, _) : _ -> error $ "Invalid .dsc file: " ++ dscName
    where
      compareVersions (name2, info2) (name1, info1) =
          case (info1, info2) of
            (Right (S.Control (para1 : _)), Right (S.Control (para2 : _))) ->
                compare (maybe Nothing (Just . V.parseDebianVersion) (S.fieldValue "Version" para1))
                        (maybe Nothing (Just . V.parseDebianVersion) (S.fieldValue "Version" para2))
            _  -> error $ "Invalid .dsc file: " ++ name1 ++ " or " ++ name2
