module BuildTarget.DebDir where

import BuildTarget
import Prelude hiding (catch)
import Debian.Types
import Debian.Types.SourceTree
import System.Directory
import Debian.IO
--import ChangeLog

-- | Get the upstream source from one location, and the debian directory from another
data DebDir = DebDir Tgt Tgt DebianSourceTree

instance Show DebDir where
    show (DebDir t q _) = "deb-dir:(" ++ show t ++ "):(" ++ show q ++ ")"

documentation = [ "deb-dir:(<target>):(<target>) - A target of this form combines two targets,"
                , "where one points to an un-debianized source tree and the other contains"
                , "a debian subdirectory." ]

instance BuildTarget DebDir where
    getTop (DebDir _ _ tree) = topdir tree
    cleanTarget (DebDir (Tgt upstream) (Tgt debian) _) path =
        do cleanTarget upstream path
           cleanTarget debian (appendPath "/debian" path)
    revision (DebDir (Tgt upstream) (Tgt debian) _) =
        do upstreamRev <- revision upstream
           case upstreamRev of
             Just rev ->
                 do debianRev <- revision debian
                    return $ maybe Nothing (\x -> Just ("deb-dir:(" ++ rev ++ "):(" ++ x ++")"))  debianRev
             Nothing ->
                 error $ "Unimplemented: no revision method for deb-dir upstream target"
    logText (DebDir _ _ _) revision = "deb-dir revision: " ++ maybe "none" id revision

prepareDebDir :: Bool -> FilePath -> Bool -> Tgt -> Tgt -> AptIO Tgt
prepareDebDir _debug top _flush (Tgt upstream) (Tgt debian) = 
    do
      --prepareDebDir style top flush upstream debian
      io $ createDirectoryIfMissing True (top ++ "/deb-dir")
      --upstream' <- prepareTree style top flush upstream
      --debian' <- prepareTree style top flush debian
      let upstreamDir = getTop upstream
      let debianDir = getTop debian
      --upstreamTree <- findSourceTree upstreamDir
      --debianTree <- findSourceTree debianDir
      -- target <- clean params style (Target base baseTree) dest
      let dest = top ++ "/deb-dir/" ++ escapeForMake ("deb-dir:(" ++ show upstream ++ "):(" ++ show debian ++ ")") 
      let cmd1 = ("set -x && rsync -aHxSpDt --delete '" ++ outsidePath upstreamDir ++ "/' '" ++ dest ++ "'")
      cleanStyle dest $ systemTask_ cmd1
      let cmd2 = ("set -x && rsync -aHxSpDt --delete '" ++ outsidePath debianDir ++ "/debian' '" ++ dest ++ "/'")
      cleanStyle dest $ systemTask_ cmd2
      tree <- findDebianSourceTree (rootEnvPath dest)
      case tree of
        Nothing -> error ("Couldn't find source tree at " ++ show dest)
        Just tree -> return $ Tgt $ DebDir (Tgt upstream) (Tgt debian) tree
    where
      cleanStyle dest = setStyle (setStart (Just (" Prepare deb-dir target in " ++ show dest)))
