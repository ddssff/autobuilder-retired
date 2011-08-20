-- |A SourceDeb target modifies another target to provide an unpacked debian source tree
-- when a debian source package is found.  A debian source package is a @.dsc@ file, a
-- @.tar.gz@ file, and an optional @.diff.gz@ file.
module Debian.AutoBuilder.BuildTarget.SourceDeb where

import qualified Debian.Control.String as S
--import Debian.Repo.Types
import qualified Debian.Version as V

import Debian.AutoBuilder.BuildTarget as BuildTarget
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Tgt (Tgt(Tgt))
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import System.Directory
import System.Unix.Process

-- | Treat the data returned by a target as a source deb.
data SourceDeb = SourceDeb Tgt FilePath String

instance Show SourceDeb where
    show (SourceDeb t _ _) = "sourcedeb:" ++ show t

documentation = [ "sourcedeb:<target> - A target of this form unpacks the source deb"
                , "retrieved by the original target and presents an unpacked source"
                , "tree for building.  Thus, the original target should retrieve a"
                , "directory containing a .dsc file, a .tar.gz, and an optional"
                , ".diff.gz file." ]

-- |SourceDeb targets inherit the revision string of the target they modify.
instance BuildTarget SourceDeb where
    getTop _ (SourceDeb (Tgt _) dir _) = dir
    revision params (SourceDeb (Tgt t) _ _) =
        BuildTarget.revision params t >>= return . ("sourcedeb:" ++)
    logText (SourceDeb (Tgt t) _ _) revision = logText t revision ++ " (source deb)"

-- |Given the BuildTarget for the base target, prepare a SourceDeb BuildTarget
-- by unpacking the source deb.
prepareSourceDeb :: P.CacheRec -> Tgt -> IO Tgt
prepareSourceDeb cache (Tgt base) =
    do let top = getTop (P.params cache) base
       dscFiles <- liftIO (getDirectoryContents top) >>=
                   return . filter (isSuffixOf ".dsc")
       dscInfo <- mapM (\ name -> liftIO (readFile (top ++ "/" ++ name) >>= return . S.parseControl name)) dscFiles
       case sortBy compareVersions (zip dscFiles dscInfo) of
         [] -> return $  error ("Invalid sourcedeb base: no .dsc file in " ++ show base)
         (dscName, Right (S.Control (dscInfo : _))) : _ ->
             do out <- liftIO (lazyCommand (unpack top dscName) L.empty)
                case exitCodeOnly out of
                  ExitSuccess -> return $ makeTarget top dscInfo dscName
                  _ -> error ("*** FAILURE: " ++ unpack top dscName)
         (dscName, _) : _ -> error ("Invalid .dsc file: " ++ dscName)
    where
      makeTarget top dscInfo dscName =
          case (S.fieldValue "Source" dscInfo, maybe Nothing (Just . V.parseDebianVersion)
                     (S.fieldValue "Version" dscInfo)) of
            (Just package, Just version) ->
                Tgt $ (SourceDeb (Tgt base) top (package ++ "-" ++ V.version version))
            _ -> error $ "Invalid .dsc file: " ++ dscName
      unpack top dscName = "cd " ++ top ++ " && dpkg-source -x " ++ dscName
      compareVersions (name2, info2) (name1, info1) =
          case (info1, info2) of
            (Right (S.Control (para1 : _)), Right (S.Control (para2 : _))) ->
                compare (maybe Nothing (Just . V.parseDebianVersion) (S.fieldValue "Version" para1))
                        (maybe Nothing (Just . V.parseDebianVersion) (S.fieldValue "Version" para2))
            _  -> error $ "Invalid .dsc file: " ++ name1 ++ " or " ++ name2
