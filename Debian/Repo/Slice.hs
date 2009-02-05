-- |Types that represent a "slice" of a repository, as defined by a
-- list of DebSource.  This is called a slice because some sections
-- may be omitted, and because different repositories may be combined
-- in the list.
module Debian.Repo.Slice
    ( sourceSlices
    , binarySlices
    , inexactPathSlices
    , releaseSlices
    , appendSliceLists
    , verifySourceLine
    , verifySourcesList
    , repoSources
    , parseNamedSliceList
    , parseNamedSliceList'
    ) where

import Control.Exception (throw)
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import Debian.Control
--import Debian.Extra.CIO (vMessage)
import Debian.Repo.IO
import Debian.Repo.LocalRepository
import Debian.Repo.Repository
--import Debian.Shell
import Debian.Repo.SourcesList
import Debian.Repo.Types
import Debian.URI
--import Extra.Net (webServerDirectoryContents)
import Extra.CIO (CIO, vPutStrBl)
--import System.Unix.Process
--import System.Directory
import Text.Regex

sourceSlices :: SliceList -> SliceList
sourceSlices = SliceList . filter ((== DebSrc) . sourceType . sliceSource) . slices

binarySlices :: SliceList -> SliceList
binarySlices = SliceList . filter ((== Deb) . sourceType . sliceSource) . slices

inexactPathSlices :: SliceList -> SliceList
inexactPathSlices = SliceList . filter (either (const False) (const True) . sourceDist . sliceSource) . slices

releaseSlices :: ReleaseName -> SliceList -> SliceList
releaseSlices release list =
    SliceList . filter (isRelease . sourceDist . sliceSource) $ (slices list)
    where isRelease = either (const False) (\ (x, _) -> x == release)

appendSliceLists :: [SliceList] -> SliceList
appendSliceLists lists =
    SliceList { slices = concat (map slices lists) }

-- |Examine the repository whose root is at the given URI and return a
-- set of sources that includes all of its releases.  This is used to
-- ensure that a package we want to upload doesn't already exist in
-- the repository.
repoSources :: CIO m => Maybe EnvRoot -> URI -> AptIOT m SliceList
repoSources chroot uri =
    do lift (vPutStrBl 3 $ "repoSources " ++ uriToString' uri)
       dirs <- lift (uriSubdirs chroot (uri {uriPath = uriPath uri ++ "/dists/"}))
       lift (vPutStrBl 3 $ "  dirs: " ++ show dirs)
       releaseFiles <- mapM (lift . readRelease uri) dirs >>= return . catMaybes
       let codenames = map (maybe Nothing (zap (flip elem dirs))) . map (fieldValue "Codename") $ releaseFiles
       lift (vPutStrBl 3 $ "  codenames: " ++ show (catMaybes codenames))
       let sections = map (maybe Nothing (Just . map parseSection' . splitRegex (mkRegex "[ \t,]+")) . fieldValue "Components") $ releaseFiles
       lift (vPutStrBl 3 $ "  sections: " ++ show (catMaybes codenames))
       let result = concat $ map sources . nubBy (\ (a, _) (b, _) -> a == b) . zip codenames $ sections
       lift (vPutStrBl 2 $ "repoSources " ++ uriToString' uri ++ " ->\n [" ++ unwords (map show result) ++ "]")
       mapM (verifyDebSource Nothing) result >>= (\ list -> return $ SliceList { slices = list })
    where
      sources (Just codename, Just components@(_ : _)) =
          [DebSource {sourceType = Deb, sourceUri = uri, sourceDist = Right (parseReleaseName codename, components)},
           DebSource {sourceType = DebSrc, sourceUri = uri, sourceDist = Right (parseReleaseName codename, components)}]
      sources _ = []
      -- Compute the list of sections for each dist on a remote server.
      zap p x = if p x then Just x else Nothing

-- |Return the list of releases in a repository, which is the
-- list of directories in the dists subdirectory.  Currently
-- this is only known to work with Apache.  Note that some of
-- the returned directories may be symlinks.
uriSubdirs :: CIO m => (Maybe EnvRoot) -> URI -> m [String]
uriSubdirs root uri =
    liftIO (dirFromURI uri') >>= either throw return
    where
      uri' = case uriScheme uri of
               "file:" -> uri {uriPath = maybe "" rootPath root ++ (uriPath uri)}
               _ -> uri

readRelease :: CIO m => URI -> String -> m (Maybe Paragraph)
readRelease uri name =
    do output <- liftIO (fileFromURI uri')
       case output of
         Left e -> throw e
         Right s -> case parseControl (show uri') (L.unpack s) of
                      Right (Control [paragraph]) -> return (Just paragraph)
                      _ -> return Nothing
    where
      uri' = uri {uriPath = uriPath uri ++ "/dists/" ++ name ++ "/Release"}

parseNamedSliceList :: CIO m => (String, String) -> AptIOT m (Maybe NamedSliceList)
parseNamedSliceList (name, text) =
    (verifySourcesList Nothing . parseSourcesList) text >>=
    \ sources -> return . Just $ NamedSliceList { sliceListName = SliceName name, sliceList = sources }

-- |Create ReleaseCache info from an entry in the config file, which
-- includes a dist name and the lines of the sources.list file.
-- This also creates the basic 
parseNamedSliceList' :: CIO m => (String, String) -> AptIOT m NamedSliceList
parseNamedSliceList' (name, text) =
    do sources <- (verifySourcesList Nothing . parseSourcesList) text
       return $ NamedSliceList { sliceListName = SliceName name, sliceList = sources }

verifySourcesList :: CIO m => Maybe EnvRoot -> [DebSource] -> AptIOT m SliceList
verifySourcesList chroot list =
    mapM (verifyDebSource chroot) list >>=
    (\ list -> return $ SliceList { slices = list })

verifySourceLine :: CIO m => Maybe EnvRoot -> String -> AptIOT m Slice
verifySourceLine chroot str = verifyDebSource chroot (parseSourceLine str)

verifyDebSource :: CIO m => Maybe EnvRoot -> DebSource -> AptIOT m Slice
verifyDebSource chroot line =
    do repo <- case uriScheme uri of
                 "file:" -> 
                     let path = EnvPath (maybe (EnvRoot "") id chroot) (uriPath uri) in
                     prepareLocalRepository path Nothing >>= return . LocalRepo
                 _ ->
                     prepareRepository uri
       return $ Slice { sliceRepo = repo, sliceSource = line }
    where
      uri = sourceUri line
