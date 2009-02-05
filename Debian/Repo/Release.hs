module Debian.Repo.Release
    ( lookupRelease
    , insertRelease
    , prepareRelease
    , findReleases
    , signRelease
    , signReleases
    , mergeReleases
    ) where

import qualified Debian.Control.String as S
import Debian.Repo.IO
import Debian.Repo.Types
import Debian.Repo.LocalRepository
import Debian.Repo.PackageIndex

import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import Data.Time
--import Data.Time.Clock.POSIX
--import System.Locale (defaultTimeLocale)
import qualified Extra.Files as EF
import qualified Extra.GPGSign as EG
import qualified Extra.Time as ET
import Extra.CIO (CIO, vPutStrBl)
import System.Directory
import System.Posix.Files
import qualified System.Posix.Files as F
import System.FilePath((</>))
import System.Unix.Process

lookupRelease :: CIO m => Repository -> ReleaseName -> AptIOT m (Maybe Release)
lookupRelease repo dist = get >>= return . findRelease repo dist

insertRelease :: CIO m => Release -> AptIOT m Release
insertRelease release =
    get >>= put . putRelease repo dist release >> return release
    where dist = releaseInfoName (releaseInfo release)
          repo = releaseRepo release

-- | Find or create a (local) release.
prepareRelease :: CIO m => LocalRepository -> ReleaseName -> [ReleaseName] -> [Section] -> [Arch] -> AptIOT m Release
prepareRelease repo dist aliases sections archList =
    -- vPutStrLn 0 ("prepareRelease " ++ name ++ ": " ++ show repo ++ " sections " ++ show sections) >>
    lookupRelease (LocalRepo repo) dist >>= maybe prepare (const prepare) -- return -- JAS - otherwise --create-section does not do anything
    where
      prepare =
          do -- FIXME: errors get discarded in the mapM calls here
	     let release = Release (LocalRepo repo) (ReleaseInfo { releaseInfoName = dist
                                                                 , releaseInfoAliases = aliases
                                                                 , releaseInfoComponents = sections
                                                                 , releaseInfoArchitectures = archList })
             -- vPutStrLn 0 ("packageIndexList: " ++ show (packageIndexList release))
             mapM (initIndex (outsidePath root)) (packageIndexList release)
             mapM (initAlias (outsidePath root) dist) aliases
             lift (writeRelease release)
	     -- This ought to be identical to repo, but the layout should be
             -- something rather than Nothing.
             repo' <- prepareLocalRepository root (repoLayout repo)
             let release' = release { releaseRepo = LocalRepo repo' }
             --vPutStrLn 0 $ "prepareRelease: prepareLocalRepository -> " ++ show repo'
             insertRelease release'
      initIndex root index = initIndexFile (root </> packageIndexDir index) (packageIndexName index)
      initIndexFile dir name =
          do io $ createDirectoryIfMissing True dir
             io $ setFileMode dir 0o040755
             ensureIndex (dir </> name)
      initAlias root dist alias = 
          io $ EF.prepareSymbolicLink (releaseName' dist) (root ++ "/dists/" ++ releaseName' alias)
      root = repoRoot repo

-- | Make sure an index file exists.
ensureIndex :: CIO m => FilePath -> AptIOT m (Either [String] ())
ensureIndex path = 
    do exists <- io $ doesFileExist path
       case exists of
         False -> io $ EF.writeAndZipFile path L.empty
         True -> return $ Right ()

signReleases :: CIO m => Maybe EG.PGPKey -> [Release] -> m ()
signReleases keyname releases = mapM_ (signRelease keyname) releases

signRelease :: CIO m => Maybe EG.PGPKey -> Release -> m ()
signRelease keyname release@(Release {releaseRepo = LocalRepo repo}) =
    do let root = repoRoot repo
       files <- writeRelease release
       case keyname of
         Nothing -> return ()
         Just key -> do results <- liftIO (EG.pgpSignFiles (outsidePath root) key files)
                        let failed = catMaybes $ map (\ (path, flag) -> if (not flag) then Just path else Nothing) (zip files results)
                        case failed of
                          [] -> return ()
                          files -> vPutStrBl 0 ("Unable to sign:\n  " ++ intercalate "\n  " files)
signRelease _keyname _release = error $ "Attempt to sign non-local repository"

-- |Write out the @Release@ files that describe a 'Release'.
writeRelease :: CIO m => Release -> m [FilePath]
writeRelease release@(Release {releaseRepo = LocalRepo repo}) =
    do let root = repoRoot repo
       indexReleaseFiles <- liftIO $ writeIndexReleases (outsidePath root) release
       masterReleaseFile <- writeMasterRelease (outsidePath root) release
       return (masterReleaseFile : indexReleaseFiles)
    where
      writeIndexReleases root release =
          mapM (writeIndex root) (packageIndexList release)
      -- It should only be necessary to write these when the component
      -- is created, not every time the index files are changed.  But
      -- for now we're doing it anyway.
      writeIndex root index =
          do let para =
                     S.Paragraph
                          [S.Field ("Archive", releaseName' . releaseInfoName . releaseInfo . packageIndexRelease $ index),
                           S.Field ("Component", sectionName' (packageIndexComponent index)),
                           S.Field ("Architecture", archName (packageIndexArch index)),
                           S.Field ("Origin", " Linspire"),
                           S.Field ("Label", " Freespire")]
             let path = packageIndexDir index ++ "/Release"
             EF.maybeWriteFile (root </> path) (show para)
             return path
      writeMasterRelease :: CIO m => FilePath -> Release -> m FilePath
      writeMasterRelease root release =
          do let paths = concat . map indexPaths $ (packageIndexList release)
             (paths', sums,sizes) <- 
                 liftIO (EG.cd root
                         (do paths' <- filterM doesFileExist paths
                             sums <-  mapM md5sum paths'
                             sizes <- mapM (liftM F.fileSize . F.getFileStatus) paths'
                             return (paths', sums, sizes)))
             let checksums = intercalate "\n" $ zipWith3 (formatFileInfo (fieldWidth sizes))
                      	   sums sizes (map (drop (1 + length (releaseDir release))) paths')
             timestamp <- liftIO (getCurrentTime >>= return . ET.formatDebianDate)
             let para = S.Paragraph [S.Field ("Origin", " Linspire"),
                                     S.Field ("Label", " Freespire"),
                                     S.Field ("Suite", " " ++ (releaseName' . releaseInfoName . releaseInfo $ release)),
                                     S.Field ("Codename", " " ++ (releaseName' . releaseInfoName . releaseInfo $ release)),
                                     S.Field ("Date", " " ++ timestamp),
                                     S.Field ("Architectures", " " ++ (intercalate " " . map archName . releaseInfoArchitectures . releaseInfo $ release)),
                                     S.Field ("Components", " " ++ (intercalate " " . map sectionName' . releaseInfoComponents . releaseInfo $ release)),
                                     S.Field ("Description", " Freespire 2.0 - Not Released"),
                                     S.Field ("Md5Sum", "\n" ++ checksums)]
             let path = "dists/" ++ (releaseName' . releaseInfoName . releaseInfo $ release) ++ "/Release"
             liftIO $ EF.maybeWriteFile (root </> path) (show para)
             return path
      indexPaths index | packageIndexArch index == Source =
          map ((packageIndexDir index) </>) ["Sources", "Sources.gz", "Sources.bz2", "Sources.diff/Index", "Release"]
      indexPaths index =
          map ((packageIndexDir index) </>) ["Packages", "Packages.gz", "Packages.bz2", "Packages.diff/Index", "Release"]
      formatFileInfo fw sum size name = intercalate " " $ ["",sum, pad ' ' fw $ show size, name]
      fieldWidth = ceiling . (logBase 10) . fromIntegral . maximum
writeRelease _release = error $ "Attempt to write release files to non-local repository"

md5sum f = 
    do output <- System.Unix.Process.processOutput "/usr/bin/md5sum" [f]
       return $ either (error $ "md5sum: missing file:" ++ f)
                (takeWhile (/= ' '))
                output

pad padchar padlen s = replicate p padchar ++ s
    where p = padlen - length s

-- Merge a list of releases so each dist only appears once
mergeReleases :: [Release] -> [Release]
mergeReleases releases =
    map (merge repos) . groupBy (==) . sortBy compare $ releases
    where
      repos = nub (map releaseRepo releases)
      merge [repo] releases =
          let aliases = map head . group . sort . concat . map (releaseInfoAliases . releaseInfo) $ releases
              components = map head . group . sort . concat . map (releaseInfoComponents . releaseInfo) $ releases
              architectures = map head . group . sort . concat . map (releaseInfoArchitectures . releaseInfo) $ releases in
          Release { releaseRepo = repo
                  , releaseInfo = ReleaseInfo { releaseInfoName = (releaseInfoName . releaseInfo . head $ releases)
                                              , releaseInfoAliases = aliases
                                              , releaseInfoComponents = components
                                              , releaseInfoArchitectures = architectures } }
      merge _ _ = error "Cannot merge releases from different repositories"

-- | Find all the releases in a repository.
findReleases :: CIO m => LocalRepository -> AptIOT m [Release]
findReleases repo@(LocalRepository _ _ releases) = mapM (findLocalRelease repo) releases

findLocalRelease :: CIO m => LocalRepository -> ReleaseInfo -> AptIOT m (Release)
findLocalRelease repo releaseInfo =
    lookupRelease (LocalRepo repo) dist >>= maybe readRelease return
    where
      readRelease =
          do let path = (outsidePath (repoRoot repo) ++ "/dists/" ++ releaseName' dist ++ "/Release")
             info <- io $ S.parseControlFromFile path
             case info of
               Right (S.Control (paragraph : _)) ->
                   case (S.fieldValue "Components" paragraph, S.fieldValue "Architectures" paragraph) of
                     (Just components, Just architectures) ->
                         let release = Release (LocalRepo repo)
                                       (ReleaseInfo
                                        { releaseInfoName = dist
                                        , releaseInfoAliases = releaseInfoAliases releaseInfo
                                        , releaseInfoComponents = map parseSection' . words $ components
                                        , releaseInfoArchitectures = map Binary . words $ architectures}) in
                         insertRelease release
                     _ -> 
                         error $ "Invalid release file: " ++ path
               _ -> error $ "Invalid release file: " ++ path
      dist = releaseInfoName releaseInfo

{-
CB: Here's my take on what needs to be done based on spelunking through the Debian repositories.

Each component (main, contrib, non-free) has a Release file in the same directory as the Packages files.  These are basically static, consisting of the following fields:

    $ cat /var/www/debian/dists/unstable/non-free/binary-i386/Release
    Archive: unstable
    Component: non-free
    Origin: Debian
    Label: Debian
    Architecture: i386

Slightly different for Source:

    $ cat /var/www/debian/dists/unstable/non-free/source/Release
    Archive: unstable
    Component: non-free
    Origin: Debian
    Label: Debian
    Architecture: source

Also, if a dist has been released, then a version number is included:

    $ cat /var/www/debian/dists/sarge/non-free/source/Release
    Archive: stable
    Version: 3.1r4
    Component: non-free
    Origin: Debian
    Label: Debian
    Architecture: source

The top level release file is different.  It has the md5sums of all the indices and release files in the distribution.  We need to find out what we should put in for Origin and Label, but I think those are company and OS respectively, so Linspire and Freespire.  It even has a codename...  As before, the version in sarge has a Version number included.

It is this top level Release file that is signed with gpg.

    $ head /var/www/debian/dists/unstable/Release
    Origin: Debian
    Label: Debian
    Suite: unstable
    Codename: sid
    Date: Wed, 06 Dec 2006 20:13:03 UTC
    Architectures: alpha amd64 arm hppa hurd-i386 i386 ia64 m68k mips mipsel powerpc s390 sparc
    Components: main contrib non-free
    Description: Debian Unstable - Not Released
    MD5Sum:
     397f3216a3a881da263a2fb5e0f8f02e 19487683 main/binary-alpha/Packages
     c3586c0dcdd6be9266a23537b386185d  5712614 main/binary-alpha/Packages.gz
     7acf2b8b938dde2c2e484425c806635a  4355026 main/binary-alpha/Packages.bz2
     dcf0d11800eb007c5435d83c853a241e     2038 main/binary-alpha/Packages.diff/Index
     3af8fbafe3e4538520989de964289712       83 main/binary-alpha/Release
     209177470d05f3a50b5217f64613ccca 19747688 main/binary-amd64/Packages
     a73fd369c6baf422621bfa7170ff1594  5777320 main/binary-amd64/Packages.gz
     380e56b5acec9ac40f160ea97ea088ef  4403167 main/binary-amd64/Packages.bz2
     43cbb8ba1631ab35ed94d43380299c38     2038 main/binary-amd64/Packages.diff/Index
     ab89118658958350f14094b1bc666a62       83 main/binary-amd64/Release
     d605623ac0d088a8bed287e0df128758 19323166 main/binary-arm/Packages


-}