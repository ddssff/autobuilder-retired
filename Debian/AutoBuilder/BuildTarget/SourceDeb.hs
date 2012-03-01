-- |A SourceDeb target modifies another target to provide an unpacked debian source tree
-- when a debian source package is found.  A debian source package is a @.dsc@ file, a
-- @.tar.gz@ file, and an optional @.diff.gz@ file.
module Debian.AutoBuilder.BuildTarget.SourceDeb where

import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Debian.AutoBuilder.BuildTarget.Common as BuildTarget
import qualified Debian.AutoBuilder.BuildTarget.Temp as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import qualified Debian.Control.String as S
import qualified Debian.Version as V
import Debian.Repo (AptIOT)
--import Debian.Repo.Types
import System.Directory
import System.Unix.Process

-- | Treat the data returned by a target as a source deb.
-- data SourceDeb = SourceDeb DL FilePath String R.RetrieveMethod

documentation = [ "sourcedeb:<target> - A target of this form unpacks the source deb"
                , "retrieved by the original target and presents an unpacked source"
                , "tree for building.  Thus, the original target should retrieve a"
                , "directory containing a .dsc file, a .tar.gz, and an optional"
                , ".diff.gz file." ]

{-
instance Download SourceDeb where
    method (SourceDeb _ _ _ m) = m
    getTop (SourceDeb _ dir _ _) = dir
    -- SourceDeb targets inherit the revision string of the target they modify.
    revision (SourceDeb t _ _ _) = "sourcedeb:" ++ BuildTarget.revision t
    logText (SourceDeb t _ _ _) revision = logText t revision ++ " (source deb)"
-}

-- |Given the BuildTarget for the base target, prepare a SourceDeb BuildTarget
-- by unpacking the source deb.
prepare :: P.CacheRec -> T.Download -> R.RetrieveMethod -> AptIOT IO T.Download
prepare _cache base m =
    do let top = getTop base
       dscFiles <- liftIO (getDirectoryContents top) >>=
                   return . filter (isSuffixOf ".dsc")
       dscInfo <- mapM (\ name -> liftIO (readFile (top ++ "/" ++ name) >>= return . S.parseControl name)) dscFiles
       case sortBy compareVersions (zip dscFiles dscInfo) of
         [] -> return $  error ("Invalid sourcedeb base: no .dsc file in " ++ show (method base))
         (dscName, Right (S.Control (dscInfo : _))) : _ ->
             do out <- liftIO (lazyCommand (unpack top dscName) L.empty)
                case exitCodeOnly out of
                  ExitSuccess -> liftIO $ makeTarget top dscInfo dscName
                  _ -> error ("*** FAILURE: " ++ unpack top dscName)
         (dscName, _) : _ -> error ("Invalid .dsc file: " ++ dscName)
    where
      makeTarget top dscInfo dscName =
          case (S.fieldValue "Source" dscInfo, maybe Nothing (Just . V.parseDebianVersion)
                     (S.fieldValue "Version" dscInfo)) of
            (Just _package, Just _version) ->
                -- (SourceDeb base top (package ++ "-" ++ V.version version) m)
                return $ T.Download {
                             T.method' = m
                           , T.getTop = top
                           , T.revision = "sourcedeb:" ++ BuildTarget.revision base
                           , T.logText = BuildTarget.revision base ++ " (source deb)"
                           , T.mVersion = Nothing
                           , T.origTarball = Nothing
                           , T.cleanTarget = \ _ -> return ([], 0)
                           , T.buildWrapper = id
                           }
            _ -> error $ "Invalid .dsc file: " ++ dscName
      unpack top dscName = "cd " ++ top ++ " && dpkg-source -x " ++ dscName
      compareVersions (name2, info2) (name1, info1) =
          case (info1, info2) of
            (Right (S.Control (para1 : _)), Right (S.Control (para2 : _))) ->
                compare (maybe Nothing (Just . V.parseDebianVersion) (S.fieldValue "Version" para1))
                        (maybe Nothing (Just . V.parseDebianVersion) (S.fieldValue "Version" para2))
            _  -> error $ "Invalid .dsc file: " ++ name1 ++ " or " ++ name2
