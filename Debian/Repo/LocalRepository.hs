module Debian.Repo.LocalRepository where

import qualified Debian.Control.ByteString as B	-- required despite warning
import qualified Debian.Control.String as S
import Debian.Repo.IO  (AptIOT, insertRepository)
import Debian.Repo.Types
--import Debian.Release
                   
import Control.Monad.Trans
import Control.Monad.State (get, put)
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe
import Debian.Repo.IO (AptState)
import Extra.CIO
import Extra.Files
import Extra.List(partitionM)
import System.FilePath
import System.Unix.Directory
import System.Directory
import System.IO
import qualified System.Posix.Files as F
import Text.Regex
  
-- | Create or update the compatibility level file for a repository.
setRepositoryCompatibility :: LocalRepository -> IO ()
setRepositoryCompatibility (LocalRepository root _ _) =
    maybeWriteFile path text
    where text = show libraryCompatibilityLevel ++ "\n"
          path = outsidePath root </> compatibilityFile

-- | Return the subdirectory where a source package with the given
-- section and name would be installed given the layout of the
-- repository.
poolDir :: LocalRepository -> Section -> String -> FilePath
poolDir (LocalRepository _ (Just Pool) _) section source =
    "pool/" ++ sectionName' section </> prefixDir </> source
    where prefixDir =
              if isPrefixOf "lib" source then
                  take (min 4 (length source)) source else
                  take (min 1 (length source)) source
poolDir (LocalRepository _ _ _) _ _ = ""

-- | Remove all the packages from the repository and then re-create
-- the empty releases.
flushLocalRepository :: CIO m => LocalRepository -> AptIOT m LocalRepository
flushLocalRepository (LocalRepository path layout _) =
    do liftIO $ removeRecursiveSafely (outsidePath path)
       prepareLocalRepository path layout

-- | Create or verify the existance of the directories which will hold
-- a repository on the local machine.  Verify the index files for each of
-- its existing releases.
prepareLocalRepository :: CIO m => EnvPath -> Maybe Layout -> AptIOT m LocalRepository
prepareLocalRepository root layout =
    do lift (vPutStrBl 3 $ "Preparing local repository at " ++ outsidePath root)
       mapM_ (liftIO . initDir)
                 [(".", 0o40755),
                  ("dists", 0o40755),
                  ("incoming", 0o41755),
                  ("removed", 0o40750),
                  ("reject", 0o40750)]
       layout' <- lift (liftIO (computeLayout (outsidePath root))) >>= (return . maybe layout Just)
                  -- >>= return . maybe (maybe (error "No layout specified for new repository") id layout) id
       mapM_ (liftIO . initDir)
                 (case layout' of
                    Just Pool -> [("pool", 0o40755), ("installed", 0o40755)]
                    Just Flat -> []
                    Nothing -> [])
       readLocalRepo root layout'
    where
      initDir (name, mode) = 
          do let path = outsidePath root </> name
             filterM (\ f -> doesDirectoryExist f >>= return . not) [path] >>=
                     mapM_ (\ f -> createDirectoryIfMissing True f)
             actualMode <- F.getFileStatus path >>= return . F.fileMode
             when (mode /= actualMode) (F.setFileMode path mode)
{-      notSymbolicLink root name =
          getSymbolicLinkStatus (root ++ "/dists/" ++ name) >>= return . not . isSymbolicLink
      hasReleaseFile root name =
          doesFileExist (root ++ "/dists/" ++ name ++ "/Release") -}

readLocalRepo :: CIO m => EnvPath -> Maybe Layout -> AptIOT m LocalRepository
readLocalRepo root layout =
    do
      state <- get
      names <- liftIO (getDirectoryContents distDir) >>=
               return . filter (\ x -> not . elem x $ [".", ".."])
      (links, dists) <- partitionM (liftIO . isSymLink . (distDir </>)) names
      linkText <- mapM (liftIO . F.readSymbolicLink) (map (distDir </>) links)
      let aliasPairs = zip linkText links ++ map (\ dist -> (dist, dist)) dists
      let distGroups = groupBy fstEq . sort $ aliasPairs
      let aliases = map (checkAliases  . partition (uncurry (==))) distGroups
      releaseInfo <- mapM (lift . getReleaseInfo) aliases
      let repo = LocalRepository { repoRoot = root
                                 , repoLayout = layout
                                 , repoReleaseInfoLocal = releaseInfo }
      put (insertRepository (repoURI repo) (LocalRepo repo) state)
      return repo
    where
      fstEq (a, _) (b, _) = a == b
      checkAliases :: ([(String, String)], [(String, String)]) -> (ReleaseName, [ReleaseName])
      checkAliases ([(realName, _)], aliases) = (parseReleaseName realName, map (parseReleaseName . snd) aliases)
      checkAliases _ = error "Symbolic link points to itself!"
      getReleaseInfo :: CIO m => (ReleaseName, [ReleaseName]) -> m ReleaseInfo
      getReleaseInfo (dist, aliases) = parseReleaseFile (releasePath dist) dist aliases
      releasePath dist = distDir </> releaseName' dist ++ "/Release"
      distDir = outsidePath root ++ "/dists"

parseReleaseFile :: CIO m => FilePath -> ReleaseName -> [ReleaseName] -> m ReleaseInfo
parseReleaseFile path dist aliases =
    do text <- liftIO (B.readFile path)
       return $ parseRelease path text dist aliases

parseRelease :: FilePath -> B.ByteString -> ReleaseName -> [ReleaseName] -> ReleaseInfo
parseRelease file text name aliases =
    case either (error . show) id (B.parseControl file text) of
      S.Control [] -> error $ "Empty release file: " ++ file
      S.Control (info : _) -> makeReleaseInfo file info name aliases

makeReleaseInfo :: FilePath -> B.Paragraph -> ReleaseName -> [ReleaseName] -> ReleaseInfo
makeReleaseInfo file info name aliases =
    case (B.fieldValue "Architectures" info, B.fieldValue "Components" info) of
      (Just archList, Just compList) ->
          case (splitRegex re (B.unpack archList), splitRegex re (B.unpack compList)) of
            (architectures@(_ : _), components@(_ : _)) ->
                ReleaseInfo { releaseInfoName = name
                            , releaseInfoAliases = aliases
                            , releaseInfoArchitectures = map Binary architectures
                            , releaseInfoComponents = map Section components }
            _ -> error $ "Invalid Architectures or Components field in Release file " ++ file
      _ -> error $ "Missing Architectures or Components field in Release file " ++ file
    where
      re = mkRegex "[ ,]+"

isSymLink path = F.getSymbolicLinkStatus path >>= return . F.isSymbolicLink

-- |Try to determine a repository's layout.
computeLayout :: FilePath -> IO (Maybe Layout)
computeLayout root =
    do
      -- If there are already .dsc files in the root directory
      -- the repository layout is Flat.
      isFlat <- getDirectoryContents root >>= return . (/= []) . catMaybes . map (matchRegex (mkRegex "\\.dsc$"))
      -- If the pool directory already exists the repository layout is
      -- Pool.
      isPool <- doesDirectoryExist (root ++ "/pool")
      case (isFlat, isPool) of
        (True, _) -> return (Just Flat)
        (False, True) -> return (Just Pool)
        _ -> return Nothing
