module Debian.Repo.SourceTree 
    ( -- * Source Tree
      SourceTreeC(..)
    , DebianSourceTreeC(..)
    , DebianBuildTreeC(..)
    , SourceTree(..)
    , DebianSourceTree(..)
    , DebianBuildTree(..)
    , findChanges
    , SourcePackageStatus(..)
    , buildDebs
    , findSourceTree
    , copySourceTree
    , findDebianSourceTree
    , copyDebianSourceTree
    , findDebianSourceTrees
    , findDebianBuildTree
    , findDebianBuildTrees
    , copyDebianBuildTree
    , findOneDebianBuildTree
    , explainSourcePackageStatus
    , addLogEntry
    --, findBuildChanges
    ) where

import Control.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import Data.Time (NominalDiffTime)
import Debian.Control.String
import Debian.Extra.CIO (printOutput)
import Debian.Shell
import Debian.Repo.Changes
import Debian.Repo.OSImage
import Debian.Repo.Types
import Debian.Shell
import qualified Debian.Version as V
import Extra.Files (replaceFile, getSubDirectories)
import Extra.List (dropPrefix)
import Extra.CIO (CIO, setStyle, addPrefixes)
import System.Directory
import System.Environment
import System.IO
import System.Unix.Process

-- |Any directory containing source code.
class Show t => SourceTreeC t where
    topdir :: t -> FilePath		-- ^The top directory of the source tree

-- |A Debian source tree, which has a debian subdirectory containing
-- at least a control file and a changelog.
class (Show t, SourceTreeC t) => DebianSourceTreeC t where
    debdir :: t -> FilePath		-- ^The directory containing the debian subdirectory
    control :: t -> Control		-- ^The contents of debian\/control
    entry :: t -> ChangeLogEntry	-- ^The latest entry from debian\/changelog

-- |A debian source tree plus a parent directory, which is where the
-- binary and source deb packages appear after a build.
class (Show t, DebianSourceTreeC t) => DebianBuildTreeC t where
    subdir :: t -> String		-- ^The basename of debdir

-- |Any directory containing source code.
data SourceTree =
    SourceTree {dir' :: FilePath} deriving Show

-- |A Debian source tree, which has a debian subdirectory containing
-- at least a control file and a changelog.
data DebianSourceTree =
    DebianSourceTree {tree' :: SourceTree,
                      control' :: Control,
                      entry' :: ChangeLogEntry} deriving Show

-- |A Debian source tree plus a parent directory, which is where the
-- binary and source deb packages appear after a build.
data DebianBuildTree =
    DebianBuildTree {topdir' :: FilePath,
                     subdir' :: String,
                     debTree' :: DebianSourceTree} deriving Show

instance SourceTreeC SourceTree where
    topdir = dir'

instance SourceTreeC DebianSourceTree where
    topdir = dir' . tree'

instance DebianSourceTreeC DebianSourceTree where
    debdir = dir' . tree'
    control = control'
    entry = entry'

instance SourceTreeC DebianBuildTree where
    topdir = topdir'

instance DebianSourceTreeC DebianBuildTree where
    debdir t = topdir' t ++ "/" ++ subdir' t
    control = control' . debTree'
    entry = entry' . debTree'

instance DebianBuildTreeC DebianBuildTree where
    subdir = subdir'

-- |Find the .changes file which is generated by a successful run of
-- dpkg-buildpackage.
findChanges :: DebianBuildTree -> IO (Either String ChangesFile)
findChanges tree =
    do let dir = topdir tree
       result <- findChangesFiles dir
       case result of
         [cf] -> return (Right cf)
         [] -> return (Left ("Couldn't find .changes file in " ++ dir))
         lst -> return (Left ("Multiple .changes files in " ++ dir ++ ": " ++ show lst))

-- |Rewrite the changelog with an added entry.
addLogEntry :: DebianSourceTreeC t => ChangeLogEntry -> t -> IO ()
addLogEntry entry debtree =
    readFile changelogPath >>= replaceFile changelogPath . ((show entry) ++)
    where
      changelogPath = (debdir debtree) ++ "/debian/changelog"

-- |There are three possible results of a build: an upload consisting
-- of only the architecture independent debs (Indep), one including
-- both indep and binary debs (All), or with a failed build (None).
data SourcePackageStatus = All | Indep | None deriving (Show, Eq)

explainSourcePackageStatus :: SourcePackageStatus -> String
explainSourcePackageStatus All = "All architecture dependent files for the current build architecture are present."
explainSourcePackageStatus Indep = "Some or all architecture-dependent files for the current build architecture are missing"
explainSourcePackageStatus None = "This version of the package is not present."

-- | Run dpkg-buildpackage in a source tree.
buildDebs :: (DebianBuildTreeC t, CIO m) => Bool -> [String] -> OSImage -> t -> SourcePackageStatus -> m (Either String NominalDiffTime)
buildDebs noClean setEnv buildOS buildTree status =
    do
      noSecretKey <- liftIO (getEnv "HOME" >>= return . (++ "/.gnupg") >>= doesDirectoryExist >>= return . not)
      -- Unset LANG so perl doesn't complain about locales.
      -- Set LOGNAME so dpkg-buildpackage doesn't die when it fails to
      -- get the original user's login information
      let buildcmd =
              "dpkg-buildpackage -sa "
              ++ (case status of Indep -> " -B "; _ -> "")
                     ++ (if noSecretKey then " -us -uc" else "")
                            ++ (if noClean then " -nc" else "")
      let fullcmd = ("chroot " ++ root ++
                     " bash -c \"unset LANG; export LOGNAME=root; " ++
                     concat (map (\ x -> "export " ++ x ++ "; ") setEnv) ++
                     "cd '" ++ fromJust (dropPrefix root path) ++ "' && " ++
                     "chmod ugo+x debian/rules && " ++
                     -- Try to build twice, some packages do configuration the first
                     -- time 'so that it is NEVER run during an automated build.' :-/
                     "{ " ++ buildcmd ++ " || " ++ buildcmd ++ " ; } "
                     ++ "\"")

      (result, elapsed) <- timeTask (liftIO (lazyCommand fullcmd L.empty) >>= setStyle (addPrefixes "[1] " "[2] ") . printOutput)
      return . checkResult (Left . (("*** FAILURE: " ++ fullcmd ++ " -> ") ++) . show) (Right elapsed) $ (discardOutput result)
    where
      path = debdir buildTree
      root = rootPath (rootDir buildOS)

-- |Make a copy of a source tree in a directory.
copySourceTree :: (SourceTreeC t, CIO m) => t -> FilePath -> m (Either String SourceTree)
copySourceTree tree dest =
    liftIO (try (createDirectoryIfMissing True dest)) >>=
    either (return . Left . show) (const (runTaskAndTest (SimpleTask 0 command))) >>=
    return . either Left (const . Right . SourceTree $ dest)
       --copyStyle $ systemTaskDR ("rsync -aHxSpDt --delete '" ++ outsidePath (topdir tree) ++ "/' '" ++ outsidePath dest ++ "'")
       --return $ SourceTree dest
    where
      command = "rsync -aHxSpDt --delete '" ++ topdir tree ++ "/' '" ++ dest ++ "'"
{-
      copyStyle = setStyle (setStart (Just ("Copying source tree (" ++ outsidePath dest ++ ")")) .
                            setError (Just ("Could not copy source tree from " ++
                                            outsidePath (topdir tree) ++ " to " ++ outsidePath dest)))
-}

copyDebianSourceTree :: (DebianSourceTreeC t, CIO m) => t -> FilePath -> m (Either String DebianSourceTree)
copyDebianSourceTree src dest =
    copySourceTree src dest >>=
    return . either Left (\ copy -> Right (DebianSourceTree copy (control src) (entry src))) 

copyDebianBuildTree :: (DebianBuildTreeC t, CIO m) => t -> FilePath -> m (Either String DebianBuildTree)
copyDebianBuildTree src dest =
    copySource >>= copyTarball >>= makeTree
    where
      copySource = copySourceTree (SourceTree . topdir $ src) dest
      copyTarball (Left message) = return (Left message)
      copyTarball (Right copy) =
          do exists <- liftIO $ doesFileExist origPath
             case exists of
               False -> return (Right copy)
               True -> runCommand 0 cmd >>= return . either Left (const (Right copy))
      makeTree (Left message) = return (Left message)
      makeTree (Right copy) =
          return $ Right (DebianBuildTree (dir' copy) (subdir src)
                          (DebianSourceTree { tree' = SourceTree { dir' = dest ++ "/" ++ subdir src }
                                            , control' = (control src)
                                            , entry' = (entry src) }))
{-               
    do copy <- copySourceTree (SourceTree . topdir $ src) dest
       exists <- io $ doesFileExist origPath
       --io $ System.IO.hPutStrLn stderr ("doesFileExist " ++ show origPath ++ " -> " ++ show exists)
       case exists of
         True -> quietRunOutputOnError cmd
         False -> return ([], noTimeDiff)
       return $ DebianBuildTree (dir' copy) (subdir src)
                  (DebianSourceTree { tree' = SourceTree { dir' = dest { envPath = envPath dest ++ "/" ++ subdir src } }
                                    , control' = (control src)
                                    , entry' = (entry src) })
    where
-}
      cmd = ("cp -p " ++ origPath ++ " " ++ dest ++ "/")
      origPath = topdir src ++ "/" ++ orig
      orig = name ++ "_" ++ version ++ ".orig.tar.gz"
      name = logPackage . entry $ src
      version = V.version . logVersion . entry $ src
      
findSourceTree :: CIO m => FilePath -> m (Either String SourceTree)
findSourceTree path =
    do exists <- liftIO $ doesDirectoryExist path
       case exists of
         False -> return . Left $ "No such directory: " ++ path
         True -> return . Right . SourceTree $ path

findDebianSourceTree :: CIO m => FilePath -> m (Either String DebianSourceTree)
findDebianSourceTree path =
    do --vPutStrLn 2 stderr $ "findDebianSourceTree " ++ show path
       findSourceTree path >>= either (return . Left) findDebianSource
    where
      findDebianSource :: CIO m => SourceTree -> m (Either String DebianSourceTree)
      findDebianSource tree@(SourceTree path) =
          do let controlPath = path ++ "/debian/control"
                 changelogPath = path ++ "/debian/changelog"
             control <-
                 liftIO (try . readFile $ controlPath) >>=
                 return . either (Left . (("Could not read control file: " ++ controlPath ++ ": ") ++) . show)
                            (either (const (Left $ "Parse error in control file: " ++ controlPath)) Right .
                                        (parseControl controlPath))
             log <- liftIO (try . readFile $ changelogPath) >>= return . either (Left . ("Failure reading changelog: " ++) . show) (Right . parseLog)
             case (control, log) of
               (Right control, (Right (Right entry : _))) -> return . Right $ DebianSourceTree tree control entry
               (Right _control, (Right (Left x : _))) -> return . Left $ "Bad changelog entry: " ++ changelogPath ++ " -> " ++ show x
               (Right _control, (Right [])) -> return . Left $ "Empty changelog file: " ++ changelogPath
               (Left control, _) -> return . Left $ "Bad control file: " ++ controlPath ++ " -> " ++ show control
               (_, Left log) -> return . Left $ "Bad changelog: " ++ changelogPath ++ " -> " ++ show log

-- |Find a DebianBuildTree inside a directory.  It finds all the
-- DebianSourceTrees, and if they all have the same package name it
-- returns the newest one according to the version numbers.  If there
-- are none, or there are trees with different package names, Nothing
-- is returned.
findOneDebianBuildTree :: CIO m => FilePath -> m (Maybe DebianBuildTree)
findOneDebianBuildTree path =
    do trees <- findDebianBuildTrees path
       case nubBy eqNames trees of
         [_] -> return $ listToMaybe (sortBy cmpVers trees)
         _ -> return Nothing
    where
      eqNames tree1 tree2 = (logPackage . entry $ tree1) == (logPackage . entry $ tree2)
      cmpVers tree1 tree2 = compare (logVersion . entry $ tree1) (logVersion . entry $ tree2)

-- |Find the DebianBuildTree in a particular subdirectory.
findDebianBuildTree :: CIO m => FilePath -> String -> m (Either String DebianBuildTree)
findDebianBuildTree path name =
    findDebianSourceTree (path ++ "/" ++ name) >>= return . either Left (Right . DebianBuildTree path name)
    

-- |Find all the debian source trees in a directory.
findDebianSourceTrees :: CIO m => FilePath -> m [(String, DebianSourceTree)]
findDebianSourceTrees path =
    do dirs <- liftIO (try (getSubDirectories path)) >>= return . either (const []) id
       trees <- mapM (\ dir -> findDebianSourceTree (path ++ "/" ++ dir)) dirs
       return $ catRightSeconds $ zip dirs trees

-- |Find all the debian source trees in a directory.
findDebianBuildTrees :: CIO m => FilePath -> m [DebianBuildTree]
findDebianBuildTrees path =
    do dirs <- (liftIO $ try (getSubDirectories path)) >>= return . either (const []) id
       trees <- mapM (\ dir -> findDebianSourceTree (path ++ "/" ++ dir)) dirs
       let trees' = catRightSeconds $ zip dirs trees
       return $ map (\ (subdir, tree) -> DebianBuildTree path subdir tree) trees'

catRightSeconds :: [(a, Either b c)] -> [(a, c)]
catRightSeconds [] = []
catRightSeconds ((y, Right x) : more) = (y, x) : catRightSeconds more
catRightSeconds ((_, _) : more) = catRightSeconds more

-- |Construct the directory name that dpkg-buildpackage expects to find the
-- source code in for this package: <packagename>-<upstreamversion>.
{-
debDir :: DebianSourceTree -> String
debDir (DebianSourceTree _ _ entry) = logPackage entry ++ "-" ++ show (logVersion entry)
-}

-- |Find the .changes file which is generated by a successful run of
-- dpkg-buildpackage.
{-
findBuildChanges :: DebianBuildTree -> IO (Either String ChangesFile)
findBuildChanges tree =
    do let dir = topdir tree
       result <- findChangesFiles dir
       case result of
         [cf] -> return (Right cf)
         [] -> return (Left ("Couldn't find .changes file in " ++ dir))
         lst -> return (Left ("Multiple .changes files in " ++ dir ++ ": " ++ show lst))
-}