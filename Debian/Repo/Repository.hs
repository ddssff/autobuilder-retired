{-# LANGUAGE ScopedTypeVariables #-}
module Debian.Repo.Repository
    ( UploadFile(..)
    , prepareRepository
    , repoArchList
    , readPkgVersion
    , showPkgVersion
    , invalidRevision
    , verifyUploadURI
    , uploadRemote
    ) where

import Control.Exception (Exception(..))
import Control.Monad.Trans
import Control.Monad.State (get, put)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import Data.Time (NominalDiffTime)
import qualified Data.Set as Set
import qualified Debian.Control.ByteString as B	-- required despite warning
import qualified Debian.Control.String as S
import Debian.Extra.CIO (tMessage, printOutput)
import Debian.Repo.Changes
import Debian.Repo.IO (AptIOT, insertRepository, lookupRepository)
import Debian.Repo.LocalRepository
import Debian.Repo.Types
import Debian.Shell
import Debian.URI
import Debian.Version
import Extra.Bool
import Extra.Either
import Extra.Files
import Extra.List
--import Extra.Net
import Extra.SSH
import Extra.CIO
import System.FilePath
import System.Unix.Process
import System.Cmd
import System.Directory
import qualified System.IO as IO
import System.IO.Unsafe
--import System.Time
import Text.Regex

-- |The file produced by dupload when a package upload attempt is made.
data UploadFile = Upload FilePath String DebianVersion Arch

-- |This is a remote repository which we have queried to find out the
-- names, sections, and supported architectures of its releases.
--data VerifiedRepo = VerifiedRepo URI [ReleaseInfo]

{- instance Show VerifiedRepo where
    show (VerifiedRepo uri _) = "Verified Repository " ++ show uri -- ++ " " ++ show dists
instance Ord VerifiedRepo where
    compare a b = compare (repoURI a) (repoURI b)
instance Eq VerifiedRepo where
    a == b = compare a b == EQ -}

-- |This is a repository whose structure we haven't examined 
-- to determine what release it contains.
--data UnverifiedRepo = UnverifiedRepo URI

{- instance Show UnverifiedRepo where
    show (UnverifiedRepo uri) = "Unverified Repository " ++ show uri -- ++ " (unverified)"
instance Ord UnverifiedRepo where
    compare a b = compare (repoURI a) (repoURI b)
instance Eq UnverifiedRepo where
    a == b = compare a b == EQ -}

-- | Prepare a repository, which may be remote or local depending on
-- the URI.
prepareRepository :: CIO m => URI -> AptIOT m Repository
prepareRepository uri =
    do state <- get
       repo <- maybe newRepo return (lookupRepository uri state)
       put (insertRepository uri repo state)
       return repo
    where
      newRepo =
             case uriScheme uri of
               "file:" -> prepareLocalRepository (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= return . LocalRepo
               -- FIXME: We only want to verifyRepository on demand.
               -- Perhaps we want to use System.IO.Unsafe.unsafeInterleaveIO?
               _ -> verifyRepository (UnverifiedRepo (show uri))
               -- _ -> return . Repository . UnverifiedRepo $ uri

{-# NOINLINE verifyRepository #-}
verifyRepository :: CIO m => Repository -> AptIOT m Repository
verifyRepository (UnverifiedRepo uri) =
    do --tio (vHPutStrBl IO.stderr 0 $ "Verifying repository " ++ show uri ++ "...")
       -- Use unsafeInterleaveIO to avoid querying the repository
       -- until the value is actually needed.
       lift (vPutStrBl 2 ("verifyRepository " ++ uri))
       releaseInfo <- do lift (vPutChar 2 '*')
                         liftIO . unsafeInterleaveIO . getReleaseInfoRemote . fromJust . parseURI $ uri
       {- tio (vHPutStrLn IO.stderr 0 $ "\n" {- -> VerifiedRepo " ++ show uri ++ " " ++ show releaseInfo -} ) -}
       return $ VerifiedRepo uri releaseInfo
verifyRepository x = return x

-- |Get the list of releases of a remote repository.
getReleaseInfoRemote :: URI -> IO [ReleaseInfo]
getReleaseInfoRemote uri =
    IO.hPutStr IO.stderr ("(verifying " ++ uriToString' uri ++ ".") >>
    dirFromURI distsURI >>=
    either (error . show) verify >>= return . catMaybes >>= 
    (\ result -> IO.hPutStr IO.stderr ")" >> return result)
    where
      distsURI = uri {uriPath = uriPath uri </> "dists/"}
      verify names =
          do let dists = map parseReleaseName names
             releaseFiles <- mapM getReleaseFile dists
             let releasePairs = zip3 (map getSuite releaseFiles) releaseFiles dists
             return $ map (uncurry3 getReleaseInfo) releasePairs
      releaseNameField releaseFile = case fmap B.unpack (B.fieldValue "Origin" releaseFile) of Just "Debian" -> "Codename"; _ -> "Suite"
      getReleaseInfo :: Maybe B.ByteString -> B.Paragraph -> ReleaseName -> Maybe ReleaseInfo
      getReleaseInfo Nothing _ _ = Nothing
      getReleaseInfo (Just dist) _ relname | (parseReleaseName (B.unpack dist)) /= relname = Nothing
      getReleaseInfo (Just dist) info _ = Just $ makeReleaseInfo "" info (parseReleaseName (B.unpack dist)) []
      getSuite releaseFile = B.fieldValue (releaseNameField releaseFile) releaseFile
      getReleaseFile :: ReleaseName -> IO (S.Paragraph' B.ByteString)
      getReleaseFile distName =
          do IO.hPutChar IO.stderr '.'
             release <- fileFromURI releaseURI >>= return . either Left (Right . B.concat . L.toChunks)
             let control = either Left (either (Left . ErrorCall . show) Right . B.parseControl (show uri)) release
             case control of
               Right (B.Control [info]) -> return info
               _ -> error ("Failed to get release info from dist " ++ show (relName distName) ++ ", uri " ++ show releaseURI)
          where
            releaseURI = distURI {uriPath = uriPath distURI </> "Release"}
            distURI = distsURI {uriPath = uriPath distsURI </> releaseName' distName}
      uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
      uncurry3 f (a, b, c) =  f a b c

-- |Make sure we can access the upload uri without typing a password.
verifyUploadURI :: CIO m => Bool -> URI -> AptIOT m ()
verifyUploadURI doExport uri =
    case doExport of
      True -> export
      False -> verify
    where
      export =
          do liftIO $ uncurry sshExport (uriDest uri)
             verify
             mkdir
      verify =
          do result <- liftIO $ uncurry sshVerify (uriDest uri)
             case result of
               False -> error $ "Unable to reach " ++ uriToString' uri ++ ", consider using --ssh-export"
               True -> return ()
             mkdir
      uriDest uri =
          let auth = maybe (error "Internal error 8") id (uriAuthority uri) in
          let port =
                  case uriPort auth of
                    (':' : number) -> Just (read number)
                    "" -> Nothing
                    x -> error $ "Internal error 9: invalid port " ++ x in
          (uriUserInfo auth ++ uriRegName auth, port)
      mkdir :: CIO m => AptIOT m ()
      mkdir =
          case uriAuthority uri of
            Nothing -> error $ "Internal error 7"
            Just auth ->
                do let cmd = "ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth  ++ " mkdir -p " ++ uriPath uri ++ "/incoming"
                   result <- liftIO $ system cmd
                   case result of
                     ExitSuccess -> return ()
                     _ -> error $ "Failure: " ++ cmd

-- | Upload all the packages in a local repository to a the incoming
-- directory of a remote repository (using dupload.)
uploadRemote :: CIO m
	     => LocalRepository		-- ^ Local repository holding the packages.
             -> URI			-- ^ URI of upload repository
             -> AptIOT m [Either String ([Output], NominalDiffTime)]
uploadRemote repo uri =
    do uploaded <- liftIO (uploadFind (outsidePath root)) >>=
                   return . Set.fromList . map uploadKey . rightOnly
       (accepted, rejected) <- liftIO (findChangesFiles (outsidePath root)) >>= return . (\x -> (x, [])) >>=
                               return . accept (notUploaded uploaded) (\ x -> (x, "Already uploaded")) >>=
                               return . rejectOlder >>=
                               acceptM (liftIO . validRevision) (\ x -> (x, "Invalid revision"))
       case rejected of
         [] -> return ()
         _ -> lift (vPutStr 0 ("Rejected:\n  " ++ consperse "\n  " (map showReject rejected) ++ "\n"))
       case accepted of
         [] -> do lift (vPutStr 0 "Nothing to upload."); return []
         _ -> do mapM (lift . dupload uri (outsidePath root)) (map Debian.Repo.Changes.path accepted)
    where
      root = repoRoot repo
      rejectOlder :: ([ChangesFile], [(ChangesFile, String)]) ->  ([ChangesFile], [(ChangesFile, String)])
      rejectOlder (accept, reject) =
          (accept', (map tag reject' ++ reject))
          where accept' = map head sortedGroups
                reject' = concat . map tail $ sortedGroups
                sortedGroups = map (sortBy compareVersions) (groupByNameAndDist accept)
                tag x = (x, "Not the newest version in incoming")
      compareVersions a b = compare (changeVersion b) (changeVersion a)
      groupByNameAndDist = groupBy equalNameAndDist . sortBy compareNameAndDist
      equalNameAndDist a b = compareNameAndDist a b == EQ
      compareNameAndDist a b =
          case compare (changePackage a) (changePackage b) of
            EQ -> compare (changeRelease a) (changeRelease b)
            x -> x
      notUploaded uploaded changes = not . Set.member (Debian.Repo.Changes.key changes) $ uploaded
      validRevision c =
          do
            let dscPath = changeDir c </> changePackage c ++ "_" ++ show (changeVersion c) ++ ".dsc"
            doesFileExist dscPath >>= cond (S.parseControlFromFile dscPath >>= either (error . show) (checkRevision dscPath)) (return True)
          where
            checkRevision _dscPath (S.Control [p]) =
                case maybe Nothing parseRevision (S.fieldValue "Revision" p) of
                    Nothing -> return False
                    Just (x, _) | x == invalidRevision -> return False
                    Just _ -> return True
            checkRevision dscPath _ = error ("Invalid .dsc file: " ++ show dscPath)
      showReject (changes, tag) = Debian.Repo.Changes.name changes ++ ": " ++ tag

uploadKey :: UploadFile -> (String, DebianVersion, Arch)
uploadKey (Upload _ name ver arch) = (name, ver, arch)

uploadLoad :: FilePath -> String -> (Either [String] UploadFile)
uploadLoad dir file =
    case parseUploadFilename file of
      Just (name, ver, arch) -> Right $ Upload dir name ver arch
      Nothing -> Left ["Couldn't parse upload filename: " ++ file]

uploadFind :: FilePath -> IO [Either [String] UploadFile]
uploadFind dir =
    getDirectoryContents dir >>=
    return . filter (isSuffixOf ".upload") >>=
    return . map (uploadLoad dir)

{-
base :: UploadFile -> String
base (Upload _ name ver arch) = name ++ "_" ++ show ver ++ "_" ++ show arch
-}

-- 		       filename     name   version   arch    ext
parseUploadFilename :: String -> Maybe (String, DebianVersion, Arch)
parseUploadFilename name =
    case matchRegex (mkRegex "^(.*/)?([^_]*)_(.*)_([^.]*)\\.upload$") name of
      Just [_, name, version, arch] -> Just (name, parseDebianVersion version, Binary arch)
      _ -> error ("Invalid .upload file name: " ++ name)

invalidRevision = "none"

-- | Parse the "Revision:" value describing the origin of the
-- package's source and the dependency versions used to build it:
--   Revision: <revisionstring> dep1=ver1 dep2=ver2 ...
parseRevision :: String -> Maybe (String, [PkgVersion])
parseRevision s =
    case words s of
      [] -> Nothing
      (revision : buildDeps) -> Just (revision, map readPkgVersion buildDeps)

showPkgVersion :: PkgVersion -> String
showPkgVersion v = show v

readPkgVersion :: String -> PkgVersion
readPkgVersion s = case mapSnd (parseDebianVersion . (drop 1)) (span (/= '=') s) of
                     (n, v) -> PkgVersion { getName = n, getVersion = v }

mapSnd f (a, b) = (a, f b)

accept :: (a -> Bool) -> (a -> (a, String)) -> ([a], [(a, String)]) -> ([a], [(a, String)])
accept p tag (accepted, rejected) =
    (accepted', map tag rejected' ++ rejected)
    where (accepted', rejected') = partition p accepted

acceptM :: (Monad m) => (a -> m Bool) -> (a -> (a, String)) -> ([a], [(a, String)]) -> m ([a], [(a, String)])
acceptM p tag (accept, reject) =
    do (accept', reject') <- partitionM p accept
       return (accept', (map tag reject' ++ reject))

-- |Run dupload on a changes file with an optional host (--to)
-- argument.
dupload :: CIO m
	=> URI		-- user
        -> FilePath	-- The directory containing the .changes file
        -> String	-- The name of the .changes file to upload
        -> m (Either String ([Output], NominalDiffTime))
dupload uri dir changesFile  =
    case uriAuthority uri of
      Nothing -> error ("Invalid Upload-URI: " ++ uriToString' uri)
      Just auth ->
          do
            let config = ("package config;\n" ++
                          "$cfg{'default'} = {\n" ++
                          "        fqdn => \"" ++ uriRegName auth ++ uriPort auth ++ "\",\n" ++
                          "        method => \"scpb\",\n" ++
	                  "        login => \"" ++ init (uriUserInfo auth) ++ "\",\n" ++
                          "        incoming => \"" ++ uriPath uri ++ "/incoming\",\n" ++
                          "        dinstall_runs => 1,\n" ++
                          "};\n\n" ++
			  "$preupload{'changes'} = '';\n\n" ++
                          "1;\n")
            liftIO $ replaceFile (dir ++ "/dupload.conf") config
            liftIO (lazyCommand (cmd changesFile) L.empty) >>=
                   tMessage ("Uploading " ++ show changesFile) >>=
                   printOutput >>=
                   dotOutput 128 >>=
                   (\ output -> timeTask (checkResult fail (return (Right output)) output)) >>=
		   (\ (result, elapsed) -> return (either Left (\ output -> Right (output, elapsed)) result))
            --style' $ runCommandQuietlyTimed (cmd changesFile)
    where
{-
      style' = setStyle (setStart (Just ("Uploading " ++ show changesFile)) .
                         setError (Just "dupload failed") .
                         setEcho True)
-}
      fail n = 
          ePutStrBl message >> return (Left message)
          where message = "dupload failed: " ++ cmd changesFile ++ " -> " ++ show n
      cmd file = "cd " ++ dir ++ " && dupload --to default -c " ++ file

repoArchList :: Repo r => r -> [Arch]
repoArchList repo =
    listIntersection (map releaseInfoArchitectures (repoReleaseInfo repo))
