module Debian.Repo.OSImage 
    ( OSImage(..)
    , prepareEnv
    , updateEnv
    , syncPool
    , chrootEnv
    , syncEnv
    , neuterEnv
    , restoreEnv
    , removeEnv
    , buildEssential
    ) where

import		 Control.Monad.Trans
import		 Control.Exception
import		 Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import		 Data.List
import		 Data.Maybe
import           Data.Time (NominalDiffTime)
import		 Debian.Extra.CIO (vMessage)
import		 Debian.Repo.Cache
import		 Debian.Repo.IO
import		 Debian.Repo.Package
import		 Debian.Relation
import		 Debian.Repo.Slice
import		 Debian.Repo.SourcesList
import		 Debian.Repo.Types
import		 Debian.Shell (timeTask, vOutput, runTaskAndTest, SimpleTask(..))
import           Debian.URI
import		 Extra.CIO (CIO, vPutStr, vPutStrBl, vBOL, ePutStr)
import		 Extra.Files (replaceFile)
import		 Extra.List (isSublistOf)
import		 Extra.Misc (sameInode, sameMd5sum)
import		 Extra.SSH (sshCopy)
import		 System.FilePath
import		 System.Unix.Directory
import		 System.Unix.Mount
import		 System.Unix.Process
import           System.Chroot (useEnv, forceList)
import		 System.Cmd
import		 System.Directory
import qualified System.IO as IO
import		 System.Posix.Files
import		 Text.Regex

-- |This type represents an OS image located at osRoot built from a
-- particular osBaseDistro using a particular osArch.  If an
-- osLocalRepo argument is given, that repository will be copied into
-- the environment and kept in sync, and lines will be added to
-- sources.list to point to it.
data OSImage
    = OS { osGlobalCacheDir :: FilePath
         , osRoot :: EnvRoot
         , osBaseDistro :: SliceList
         , osReleaseName :: ReleaseName
         , osArch :: Arch
	 -- | The associated local repository, where packages we
         -- build inside this image are first uploaded to.
         , osLocalRepoMaster :: Maybe LocalRepository
         -- |A copy of osLocalRepo which is inside the changeroot
         --, osLocalRepoCopy :: Maybe LocalRepo
         -- | Update and return a copy of the local repository
         -- which is inside the changeroot.
         , osSourcePackages :: [SourcePackage]
         , osBinaryPackages :: [BinaryPackage]
         }

instance Show OSImage where
    show os = intercalate " " ["OS {",
                               rootPath (osRoot os),
                               relName (osReleaseName os),
                               archName (osArch os),
                               show (osLocalRepoMaster os)]

instance Ord OSImage where
    compare a b = case compare (osRoot a) (osRoot b) of
                    EQ -> case compare (osBaseDistro a) (osBaseDistro b) of
                            EQ -> compare (osArch a) (osArch b)
                            x -> x
                    x -> x

instance Eq OSImage where
    a == b = compare a b == EQ

instance AptCache OSImage where
    globalCacheDir = osGlobalCacheDir
    rootDir = osRoot
    aptArch = osArch 
    -- aptSliceList = osFullDistro
    aptBaseSliceList = osBaseDistro
    aptSourcePackages = osSourcePackages                  
    aptBinaryPackages = osBinaryPackages
    aptReleaseName = osReleaseName

instance AptBuildCache OSImage where
    aptSliceList = osFullDistro

-- |The sources.list is the list associated with the distro name, plus
-- the local sources where we deposit newly built packages.
osFullDistro :: OSImage -> SliceList
osFullDistro os = SliceList { slices = slices (osBaseDistro os) ++ slices (localSources os) }

localSources :: OSImage -> SliceList
localSources os =
    case osLocalRepoMaster os of
      Nothing -> SliceList { slices = [] }
      Just repo ->
          let repo' = repoCD (EnvPath (envRoot (repoRoot repo)) "/work/localpool") repo in
          let name = relName (osReleaseName os) in
          let src = DebSource Deb (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"]))
              bin = DebSource DebSrc (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"])) in
          SliceList { slices = [Slice { sliceRepo = LocalRepo repo', sliceSource = src }, 
                                Slice { sliceRepo = LocalRepo repo', sliceSource = bin }] }

-- |Change the root directory of a repository.  FIXME: This should
-- also sync the repository to ensure consistency.
repoCD :: EnvPath -> LocalRepository -> LocalRepository
repoCD path repo = repo { repoRoot = path }

getSourcePackages :: CIO m => OSImage -> AptIOT m [SourcePackage]
getSourcePackages os =
    mapM (sourcePackagesOfIndex' os) indexes >>= return . concat
    where indexes = concat . map (sliceIndexes os) . slices . sourceSlices . aptSliceList $ os

getBinaryPackages :: CIO m => OSImage -> AptIOT m [BinaryPackage]
getBinaryPackages os =
    mapM (binaryPackagesOfIndex' os) indexes >>= return . concat
    where indexes = concat . map (sliceIndexes os) . slices . binarySlices . aptSliceList $ os

data UpdateError
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
    | Flushed
    deriving Show

-- |Create or update an OS image in which packages can be built.
prepareEnv :: CIO m
           => FilePath
           -> EnvRoot			-- ^ The location where image is to be built
           -> NamedSliceList		-- ^ The sources.list
           -> Maybe LocalRepository	-- ^ The associated local repository, where newly
					-- built packages are stored.  This repository is
					-- periodically copied into the build environment
					-- so apt can access the packages in it.
           -> Bool			-- ^ If true, remove and rebuild the image
           -> SourcesChangedAction	-- ^ What to do if called with a sources.list that
					-- differs from the previous call (unimplemented)
           -> [String]			-- ^ Extra packages to treat as essential
           -> [String]			-- ^ Packages to consider non-essential even if marked essential  
           -> [String]			-- ^ Extra packages to install during the build
           -> AptIOT m OSImage
prepareEnv cacheDir root distro repo flush ifSourcesChanged extraEssential omitEssential extra =
    do arch <- liftIO buildArchOfRoot
       --vPutStrLn 0 $ "prepareEnv repo: " ++ show repo
       let os = OS { osGlobalCacheDir = cacheDir
                   , osRoot = root
                   , osBaseDistro = sliceList distro
                   , osReleaseName = ReleaseName . sliceName . sliceListName $ distro
                   , osArch = arch
                   , osLocalRepoMaster = repo
                   , osSourcePackages = []
                   , osBinaryPackages = [] }
       update os >>= recreate arch os >>= lift . syncPool
    where
      update _ | flush = return (Left Flushed)
      update os = updateEnv os
      recreate _ _ (Right os) = return os
      recreate arch os (Left (Changed name path computed installed))
          | ifSourcesChanged == SourcesChangedError =
              error $ "FATAL: Sources for " ++ relName name ++ " in " ++ path ++
                       " don't match computed configuration.\n\ncomputed:\n" ++
                       show computed ++ "\ninstalled:\n" ++ 
                       show installed
      recreate arch os (Left reason) =
          do lift (vPutStrBl 0 $ "Removing and recreating build environment at " ++ rootPath root ++ ": " ++ show reason)
             lift (vPutStrBl 2 ("removeRecursiveSafely " ++ rootPath root))
             liftIO (removeRecursiveSafely (rootPath root))
             lift (vPutStrBl 2 ("createDirectoryIfMissing True " ++ show (distDir os)))
             liftIO (createDirectoryIfMissing True (distDir os))
             lift (vPutStrBl 3 ("writeFile " ++ show (sourcesPath os) ++ " " ++ show (show . osBaseDistro $ os)))
             liftIO (replaceFile (sourcesPath os) (show . osBaseDistro $ os))
             buildEnv cacheDir root distro arch repo extraEssential omitEssential extra >>= lift . neuterEnv >>= lift . syncPool

-- |Prepare a minimal \/dev directory
prepareDevs :: FilePath -> IO ()
prepareDevs root = do
  mapM_ prepareDev 
            ([(root ++ "/dev/null", "c", 1, 3),
              (root ++ "/dev/zero", "c", 1, 5),
              (root ++ "/dev/full", "c", 1, 7),
              (root ++ "/dev/console", "c", 5, 1),
              (root ++ "/dev/random", "c", 1, 8),
              (root ++ "/dev/urandom", "c", 1, 9)] ++
             (map (\ n -> (root ++ "/dev/loop" ++ show n, "b", 7, n)) [0..7]) ++
             (map (\ n -> (root ++ "/dev/loop/" ++ show n, "b", 7, n)) [0..7]))
  where
    prepareDev (path, typ, major, minor) = do
                     createDirectoryIfMissing True (fst (splitFileName path))
                     let cmd = "mknod " ++ path ++ " " ++ typ ++ " " ++ show major ++ " " ++ show minor
                     exists <- doesFileExist path
                     if not exists then
                         system cmd else
                         return ExitSuccess

-- Create a new clean build environment in root.clean
-- FIXME: create an ".incomplete" flag and remove it when build-env succeeds
buildEnv :: CIO m
         => FilePath
         -> EnvRoot
         -> NamedSliceList
         -> Arch
         -> Maybe LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> AptIOT m OSImage
buildEnv cacheDir root distro arch repo extraEssential omitEssential extra =
    do
      -- We can't create the environment if the sources.list has any
      -- file:// URIs because they can't yet be visible inside the
      -- environment.  So we grep them out, create the environment, and
      -- then add them back in.
      (output, result) <-
          liftIO (lazyCommand cmd L.empty) >>=
          lift . vMessage 0 ("Creating clean build environment (" ++ sliceName (sliceListName distro) ++ ")") >>=
          lift . vMessage 1 ("# " ++ cmd) >>=
          lift . vOutput 1 >>=
          return . collectStderr . mergeToStderr
      case result of
        -- It is fatal if we can't build the environment
        [Result ExitSuccess] ->
            do lift (ePutStr "done.\n")
               let os = OS { osGlobalCacheDir = cacheDir
                           , osRoot = root
                           , osBaseDistro = sliceList distro
                           , osReleaseName = ReleaseName . sliceName . sliceListName $ distro
                           , osArch = arch
                           , osLocalRepoMaster = repo
                           , osSourcePackages = []
                           , osBinaryPackages = [] }
               let sourcesPath = rootPath root ++ "/etc/apt/sources.list"
               -- Rewrite the sources.list with the local pool added.
               liftIO $ replaceFile sourcesPath (show . aptSliceList $ os)
               updateEnv os >>= either (error . show) return
        failure ->
            (lift . ePutStr . L.unpack $ output) >>
            error ("Could not create build environment:\n " ++ cmd ++ " -> " ++ show failure)
    where
      cmd = ("unset LANG; build-env --allow-missing-indexes --immediate-configure-false " ++
             " -o " ++ rootPath root ++ " -s " ++ cacheSourcesPath cacheDir (ReleaseName (sliceName (sliceListName distro))) ++
             " --with '" ++ intercalate " " extra ++ "'" ++
             " --with-essential '" ++ intercalate " " extraEssential ++ "'" ++
             " --omit-essential '" ++ intercalate " " omitEssential ++ "'")

-- |Try to update an existing build environment: run apt-get update
-- and dist-upgrade.
updateEnv :: CIO m => OSImage -> AptIOT m (Either UpdateError OSImage)
updateEnv os =
    do verified <- verifySources os
       case verified of
         Left x -> return $ Left x
         Right _ ->
             do liftIO $ prepareDevs (rootPath root)
                os' <- lift $ syncPool os
                liftIO $ sshCopy (rootPath root)
                source <- getSourcePackages os'
                binary <- getBinaryPackages os'
                return . Right $ os' {osSourcePackages = source, osBinaryPackages = binary}
    where
      verifySources :: CIO m => OSImage -> AptIOT m (Either UpdateError OSImage)
      verifySources os =
          do let computed = remoteOnly (aptSliceList os)
                 sourcesPath = rootPath root ++ "/etc/apt/sources.list"
             text <- liftIO (try $ readFile sourcesPath)
             installed <-
                 case text of
                   Left _ -> return Nothing
                   Right s -> verifySourcesList (Just root) (parseSourcesList s) >>= return . Just . remoteOnly
             case installed of
               Nothing -> return $ Left $ Missing (osReleaseName os) sourcesPath
               Just installed
                   | installed /= computed ->
                       return $ Left $ Changed (osReleaseName os) sourcesPath computed installed
               _ -> return $ Right os
      root = osRoot os
      remoteOnly :: SliceList -> SliceList
      remoteOnly x = x {slices = filter r (slices x)} where r x = (uriScheme . sourceUri . sliceSource $ x) == "file:"

chrootEnv :: OSImage -> EnvRoot -> OSImage
chrootEnv os dst = os {osRoot=dst}

-- Sync the environment from the clean copy.  All this does besides
-- performing the proper rsync command is to make sure the destination
-- directory exists, otherwise rsync will fail.  Not sure why the 'work'
-- subdir is appended.  There must have been a reason at one point.
syncEnv :: CIO m => OSImage -> OSImage -> m OSImage
syncEnv src dst =
    mkdir >>= liftIO . umount >>= sync >>= either (error . show) (const (return dst))
{-
    either (return . Left . show) (const (liftIO doUmounts)) >>=
    either (return . Left) (const . syncStyle . runCommand 1 $ cmd) >>=
    either (error . show) (const (return dst))
-}
    where
      mkdir = liftIO (try (createDirectoryIfMissing True (rootPath (osRoot dst) ++ "/work")))
      umount (Left message) = return . Left . show $ message
      umount (Right _) =
          do srcResult <- umountBelow (rootPath (osRoot src))
             dstResult <- umountBelow (rootPath (osRoot dst))
             case filter (\ (_, (_, _, code)) -> code /= ExitSuccess) (srcResult ++ dstResult) of
               [] -> return (Right ())
               failed -> return . Left $ "umount failure(s): " ++ show failed
      sync (Left message) = return (Left message)
      sync (Right _) =
          runTaskAndTest (SimpleTask 1 cmd) >>=
          vMessage 0 ("Copying clean build environment: " ++
                      rootPath (osRoot src) ++ " -> " ++ rootPath (osRoot dst))
      cmd = ("rsync -aHxSpDt '--exclude=/work/build/**' --delete '" ++ rootPath (osRoot src) ++
             "/' '" ++ rootPath (osRoot dst) ++ "'")
{-
      syncStyle = setStyle (setStart (Just ("Copying clean build environment: " ++
                                            rootPath (osRoot src) ++ " -> " ++ rootPath (osRoot dst))) .
                            setError (Just "Could not sync with clean build environment"))
-}

-- |To "neuter" an executable is to replace it with a hard link to
-- \/bin\/true in such a way that the operation can be reversed.  This
-- is done in order to make it safe to install files into it when it
-- isn't "live".  If this operation fails it is assumed that the
-- image is damaged, so it is removed.
neuterEnv :: CIO m => OSImage -> m OSImage
neuterEnv os =
    do
      vBOL 0 >> vPutStr 0 ("Neutering OS image (" ++ stripDist (rootPath root) ++ ")...")
      result <- liftIO $ try $ mapM_ (neuterFile os) neuterFiles
      either (\ e -> error $ "Failed to neuter environment " ++ rootPath root ++ ": " ++ show e)
             (\ _ -> return os)
             result
    where
      root = osRoot os

neuterFiles :: [(FilePath, Bool)]
neuterFiles = [("/sbin/start-stop-daemon", True),
	       ("/usr/sbin/invoke-rc.d", True),
	       ("/sbin/init",False),
	       ("/usr/sbin/policy-rc.d", False)]

-- neuter_file from build-env.ml
neuterFile :: OSImage -> (FilePath, Bool) -> IO ()
neuterFile os (file, mustExist) =
    do
      -- putStrBl ("Neutering file " ++ file)
      exists <- doesFileExist (outsidePath fullPath)
      if exists then
          neuterExistantFile else
          if mustExist then
              error ("Can't neuter nonexistant file: " ++ outsidePath fullPath) else
              return () -- putStrBl "File doesn't exist, nothing to do"

    where
      neuterExistantFile =
          do
            sameFile <- sameInode (outsidePath fullPath) (outsidePath binTrue)
            if sameFile then
                return () else -- putStrBl "File already neutered"
                neuterUnneuteredFile
      neuterUnneuteredFile =
          do
            hasReal <- doesFileExist (outsidePath fullPath ++ ".real")
            if hasReal then
                neuterFileWithRealVersion else
                neuterFileWithoutRealVersion
            createLink (outsidePath binTrue) (outsidePath fullPath)
      neuterFileWithRealVersion =
          do
            sameCksum <- sameMd5sum (outsidePath fullPath) (outsidePath fullPath ++ ".real")
            if sameCksum then
                removeFile (outsidePath fullPath) else
                error (file ++ " and " ++ file ++ ".real differ (in " ++ rootPath root ++ ")")
                           
      neuterFileWithoutRealVersion = renameFile (outsidePath fullPath) (outsidePath fullPath ++ ".real")

      fullPath = EnvPath root file
      binTrue = EnvPath root "/bin/true"
      root = osRoot os

-- |Reverse the neuterEnv operation.
restoreEnv :: OSImage -> IO OSImage
restoreEnv os =
    do
      IO.hPutStr IO.stderr "De-neutering OS image..."
      result <- try $ mapM_ (restoreFile os) neuterFiles
      either (\ e -> error $ "damaged environment " ++ rootPath root ++ ": " ++ show e ++ "\n  please remove it.")
                 (\ _ -> return os) result
    where
      root = osRoot os

-- check_and_restore from build-env.ml
restoreFile :: OSImage -> (FilePath, Bool) -> IO ()
restoreFile os (file, mustExist) =
    do
      exists <- doesFileExist (outsidePath fullPath)
      if exists then
          restoreExistantFile else
          if mustExist then
              error ("Can't restore nonexistant file: " ++ outsidePath fullPath) else
              return ()
    where
      restoreExistantFile =
          do
            isTrue <- sameInode (outsidePath fullPath) (outsidePath binTrue)
            hasReal <- doesFileExist (outsidePath fullPath ++ ".real")
            case (isTrue, hasReal) of
              (True, True) ->
                  do
                    removeFile (outsidePath fullPath)
                    renameFile (outsidePath fullPath ++ ".real") (outsidePath fullPath)
              (False, _) -> error "Can't restore file not linked to /bin/true"
              (_, False) -> error "Can't restore file with no .real version"

      fullPath = EnvPath root file
      binTrue = EnvPath root "/bin/true"
      root = osRoot os

-----------------------------------

-- |Build the dependency relations for the build essential packages.
-- For this to work the build-essential package must be installed in
-- the OSImage.
buildEssential :: OSImage -> Bool -> IO Relations
buildEssential _ True = return []
buildEssential os False =
    do
      essential <-
          readFile (rootPath root ++ "/usr/share/build-essential/essential-packages-list") >>=
          return . lines >>= return . dropWhile (/= "") >>= return . tail >>= return . filter (/= "sysvinit") >>=
          return . parseRelations . (intercalate ", ") >>=
          return . (either (error "parse error in /usr/share/build-essential/essential-packages-list") id)
      let re = mkRegex "^[^ \t]"
      relationText <-
          readFile (rootPath root ++ "/usr/share/build-essential/list") >>=
          return . lines >>=
          return . dropWhile (/= "BEGIN LIST OF PACKAGES") >>= return . tail >>=
          return . takeWhile (/= "END LIST OF PACKAGES") >>=
          return . filter ((/= Nothing) . (matchRegex re))
      -- ePut ("buildEssentialText: " ++ intercalate ", " relationText)
      let buildEssential = parseRelations (intercalate ", " relationText)
      let buildEssential' = either (\ l -> error ("parse error in /usr/share/build-essential/list:\n" ++ show l)) id buildEssential
      return (essential ++ buildEssential')
    where
      root = osRoot os

-- |Remove an image.  The removeRecursiveSafely function is used to
-- ensure that any file systems mounted inside the image are unmounted
-- instead of destroyed.
removeEnv :: OSImage -> IO ()
removeEnv os =
    do
      IO.hPutStr IO.stderr "Removing build environment..."
      removeRecursiveSafely (rootPath root)
      IO.hPutStrLn IO.stderr "done."
    where
      root = osRoot os

-- |Use rsync to synchronize the pool of locally built packages from 
-- outside the build environment to the location inside the environment
-- where apt can see and install the packages.
syncPool :: CIO m => OSImage -> m OSImage
syncPool os =
    case osLocalRepoMaster os of
      Nothing -> return os
      Just repo ->
          liftIO (try (createDirectoryIfMissing True (rootPath root ++ "/work"))) >>=
          either (return . Left . show) (const (rsync repo)) >>=
          either (return . Left) (const (updateLists os)) >>=
          either (error . show) (const (return os))
    where
      rsync repo =
          liftIO (lazyCommand (cmd repo) L.empty) >>=
               	      vOutput 0 >>=
                      vMessage 1 ("Syncing local pool from " ++ outsidePath (repoRoot repo) ++ " -> " ++ rootPath root) >>= 
                      checkResult (\ n -> return (Left $ "*** FAILURE syncing local pool: " ++ cmd repo ++ " -> " ++ show n)) (return (Right ()))
      cmd repo = "rsync -aHxSpDt --delete '" ++ outsidePath (repoRoot repo) ++ "/' '" ++ rootPath root ++ "/work/localpool'"
      root = osRoot os

updateLists :: CIO m => OSImage -> m (Either String NominalDiffTime)
updateLists os =
    do vMessage 1 ("Updating OSImage " ++ stripDist (rootPath root) ++ " ") ()
       vMessage 2 ("# " ++ cmd) ()
       ((_out, err, code), elapsed) <- liftIO . timeTask $ useEnv (rootPath root) forceList (lazyCommand cmd L.empty) >>= return . collectOutputUnpacked
       return $ case code of
                  [ExitSuccess] -> Right elapsed
                  result -> Left $ "*** FAILURE: Could not update environment: " ++ cmd ++ " -> " ++ show result ++ "\n" ++ err
    where
      cmd = "unset LANG; apt-get update && apt-get -y --force-yes dist-upgrade"
      root = osRoot os
    
stripDist :: FilePath -> FilePath
stripDist path = maybe path (\ n -> drop (n + 7) path) (isSublistOf "/dists/" path)
