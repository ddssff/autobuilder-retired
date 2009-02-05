{-# LANGUAGE DeriveDataTypeable #-}
module Debian.Apt.Index
    ( update
    , Fetcher
    , CheckSums(..)
    , Compression(..)
    , FileTuple
    , Size
    , controlFromIndex
    , controlFromIndex'
    , findContentsFiles
    , findIndexes
    , indexesInRelease
    , tupleFromFilePath
    ) where

import Control.Monad
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Time
import Debian.Apt.Methods
import Debian.Control (formatControl)
import Debian.Control.ByteString
import Debian.Control.Common
import Debian.Repo.Types
import Debian.URI
import System.Directory
import System.FilePath ((</>))
import System.Posix.Files
import System.Unix.FilePath
import qualified System.Unix.Misc as Misc
import Text.ParserCombinators.Parsec.Error


-- |Package indexes on the server are uncompressed or compressed with
-- gzip or bzip2. We do not know what will exist on the server until we
-- actually look. This type is used to mark the compression status of
-- what was actually found.
data Compression
    = BZ2 | GZ | Uncompressed
      deriving (Read, Show, Eq, Ord, Enum, Bounded)

data CheckSums 
    = CheckSums { md5sum :: Maybe String
                , sha1   :: Maybe String
                , sha256 :: Maybe String
                }
      deriving (Read, Show, Eq)

-- |function-type for a function that downloads a file
-- The timestamp is optional. If the local file is as new or newer
-- than the remote copy, the download may be skipped.
--
-- A good choice might be a partially parameterized call to
-- 'Debian.Apt.Methods.fetch'
type Fetcher =
    URI ->		-- remote URI
    FilePath ->		-- local file name
    Maybe UTCTime ->	-- optional time stamp for local file
    IO Bool		-- True on success, False on failure

-- |update - similar to apt-get update

-- downloads the index files associated with a sources.list. The
-- downloaded index files will have the same basenames that apt-get uses
-- in \/var\/lib\/apt\/lists. You can almost use this function instead of
-- calling apt-get update. However there are a few key differences:
--  1. apt-get update also updates the binary cache files
--  2. apt-get update uses the partial directory and lock file in\ /var\/lib\/apt\/lists 
--  3. apt-get update downloads the Release and Release.gpg files
update :: Fetcher -- ^ function that will do actually downloading
       -> FilePath -- ^ download indexes to the directory (must already exist)
       -> String -- ^ binary architecture
       -> [DebSource] -- ^ sources.list 
       -> IO [Maybe (FilePath, Compression)] -- ^ (basename of index file, compression status)
update fetcher basePath arch sourcesList =
    mapM (uncurry $ fetchIndex fetcher) (map (\(uri, fp, _) -> (uri, (basePath </> fp))) (concatMap (indexURIs arch) sourcesList))

-- | download possibly compressed files
-- NOTE: index uri must not include the .bz2 or .gz extension
fetchIndex :: Fetcher -- ^ function that will do the actual fetch
           -> URI -- ^ remote URI of package index, without .bz2 or .gz extension
           -> FilePath -- ^ name to save downloaded file as, without .bz2 or .gz extension
           -> IO (Maybe (FilePath, Compression)) -- ^ (downloaded file name + extension, compression status)
fetchIndex fetcher uri localPath =
    do let localPath' = localPath ++ ".bz2"
       --lm <- getLastModified localPath'
       res <- fetcher (uri { uriPath = (uriPath uri) ++ ".bz2" }) localPath' Nothing
       return Nothing
       if res
          then return $ Just (localPath', BZ2)
          else do let localPath' = localPath ++ ".gz"
                  lm <- getLastModified localPath'
                  res <- fetcher (uri { uriPath = (uriPath uri) ++ ".gz" }) localPath' lm
                  if res
                     then return $ Just (localPath', GZ)
                     else do lm <- getLastModified localPath
                             res <- fetcher (uri { uriPath = (uriPath uri) }) localPath lm
                             if res
                                then return (Just (localPath, Uncompressed))
                                else return Nothing

-- |examine a DebSource line, and calculate for each section:
--  - the URI to the uncompressed index file
--  - the basename that apt-get would name the downloaded index
-- FIXME: ExactPath dist will fail with error at runtime :(
indexURIs :: String -- ^ which binary architecture
          -> DebSource -- ^ line from sources.list
          -> [(URI, FilePath, DebSource)] -- ^ (remote uri, local name, deb source for just this section)
indexURIs arch debSource =
    map (\ section -> let (uri, fp) = calcPath (sourceType debSource) arch baseURI release section
                      in (uri,fp, debSource { sourceDist = (Right (release, [section])) }) ) sections
    where
      baseURI = sourceUri debSource
      (release, sections) =
          either (error $ "indexURIs: support not implemented for exact path: " ++ show debSource) id (sourceDist debSource)

-- |return a tuple for the section 
--  - the URI to the uncompressed index file
--  - the basename that apt-get uses for the downloaded index
-- FIXME: support for Release and Release.gpg
calcPath :: SourceType -- ^ do we want Packages or Sources
         -> String  -- ^ The binary architecture to use for Packages
         -> URI -- ^ base URI as it appears in sources.list
         -> ReleaseName -- ^ the release (e.g., unstable, testing, stable, sid, etc)
         -> Section -- ^ the section (main, contrib, non-free, etc)
         -> (URI, [Char]) -- ^ (uri to index file, basename for the downloaded file)
calcPath srcType arch baseURI release section =
          let indexPath = case srcType of
                      DebSrc -> "source/Sources"
                      Deb -> "binary-" ++ arch </> "Packages"
              path = (uriPath baseURI) </> "dists" </> (releaseName' release) </> sectionName' section </> indexPath
          in (baseURI { uriPath = path }, addPrefix . escapePath $ path)
          where
            addPrefix s = prefix scheme user' pass' reg port ++ {- "_" ++ -} s
            prefix "http:" (Just user) Nothing (Just host) port = user ++ host ++ port
            prefix "http:" _ _ (Just host) port = host ++ port
            prefix "ftp:" _ _ (Just host) _ = host
            prefix "file:" Nothing Nothing Nothing "" = ""
            prefix "ssh:" (Just user) Nothing (Just host) port = user ++ host ++ port
            prefix "ssh:" _ _ (Just host) port = host ++ port
            prefix _ _ _ _ _ = error ("calcPath: unsupported uri: " ++ uriToString' baseURI)
            user' = maybeOfString user
            pass' = maybeOfString pass
            (user, pass) = break (== ':') userpass
            userpass = maybe "" uriUserInfo auth
            reg = maybeOfString $ maybe "" uriRegName auth
            port = maybe "" uriPort auth
            scheme = uriScheme baseURI
            auth = uriAuthority baseURI
            --path = uriPath baseURI

            escapePath :: String -> String
            escapePath s = intercalate "_" $ wordsBy (== '/') s

            maybeOfString :: String -> Maybe String
            maybeOfString "" = Nothing
            maybeOfString s = Just s

            wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
            wordsBy p s = 
                case (break p s) of
                  (s, []) -> [s]
                  (h, t) -> h : wordsBy p (drop 1 t)

-- |Parse a possibly compressed index file.
controlFromIndex :: Compression -> FilePath -> L.ByteString -> Either ParseError (Control' B.ByteString)
controlFromIndex GZ path s = parseControl path . B.concat . L.toChunks . GZip.decompress $ s
controlFromIndex BZ2 path s = parseControl path . B.concat . L.toChunks . BZip.decompress $ s
controlFromIndex Uncompressed path s = parseControl path . B.concat . L.toChunks $ s

-- |parse an index possibly compressed file 
controlFromIndex' :: Compression -> FilePath -> IO (Either ParseError (Control' B.ByteString))
controlFromIndex' compression path = L.readFile path >>= return . controlFromIndex compression path

type Size = Integer
type FileTuple = (CheckSums, Size, FilePath) 

-- |A release file contains a list of indexes (Packages\/Sources). Each
-- Package or Source index may appear multiple times because it may be
-- compressed several different ways. This function will return an
-- assoc list where the key is the name of the uncompressed package
-- index name and the value is the list of (file, compression) which
-- decompress to the key.
groupIndexes :: [FileTuple] -> [(FilePath, [(FileTuple, Compression)])]
groupIndexes indexFiles =
    M.toList $ M.fromListWith combine $ map makeKV indexFiles
    where
      makeKV fileTuple@(_,_,fp) =
          let (name, compressionMethod) = uncompressedName fp
          in
            (name, [(fileTuple, compressionMethod)])
      combine = (\x y -> sortBy (compare `on` snd) (x ++ y))
{-
      with t@(_,_,fp) m =
          let (un, compression) = 
          in
            M.insertWith
-}

{-
groupIndexes' :: String ->[FileTuple] -> [(FilePath, [(FileTuple, Compression)])]
groupIndexes' iType indexFiles =
    M.toList (foldr (insertType iType) M.empty indexFiles)
    where
      insertType iType t@(_,_,fp) m =
          case uncompressedName' iType fp of
            Nothing -> m
            (Just (un, compression)) ->
                M.insertWith (\x y -> sortBy (compare `on` snd) (x ++ y)) un [(t, compression)] m
-}

-- |The release file contains the checksums for the uncompressed
-- package indexes, even if the uncompressed package indexes are not
-- stored on the server. This function returns the list of files that
-- actually exist.
filterExists :: FilePath -> (FilePath, [(FileTuple, Compression)]) -> IO (FilePath, [(FileTuple, Compression)])
filterExists distDir (fp, alternatives) =
          do e <- filterM ( \((_,_,fp),_) -> fileExist (distDir </> fp)) alternatives
             -- when (null e) (error $ "None of these files exist: " ++ show alternatives)
             return (fp, e)

findIndexes :: FilePath -> String -> [FileTuple] -> IO [(FileTuple, Compression)]
findIndexes distDir iType controlFiles =
    let indexes = groupIndexes controlFiles
    in
      do indexes' <- mapM (filterExists distDir) (filter (isType iType) indexes)
         return $ map (head . snd) (filter (not . null . snd) indexes')
    where
      isType iType (fp, _) = iType `isSuffixOf` fp 

{-
findIndexes' :: FilePath -> String -> [FileTuple] -> IO [(FileTuple, Compression)]
findIndexes' distDir iType controlFiles =
    let m = groupIndexes' iType controlFiles
    in
      do m' <- mapM (filterExists distDir) m
         return $ map (head . snd) (filter (not . null . snd) m')
-}

      -- insertType :: String -> (CheckSums, Integer, FilePath) -> M.Map FilePath ((CheckSums, Integer, FilePath), Compression) -> M.Map FilePath ((CheckSums, Integer, FilePath), Compression)

{-
uncompressedName' :: String -> FilePath -> Maybe (FilePath, Compression)
uncompressedName' iType fp
          | isSuffixOf iType fp = Just (fp, Uncompressed)
          | isSuffixOf (iType ++".gz") fp = Just (reverse . (drop 3) . reverse $ fp, GZ)
          | isSuffixOf (iType ++".bz2") fp = Just (reverse . (drop 4) . reverse $ fp, BZ2)
          | otherwise = Nothing
-}

uncompressedName :: FilePath -> (FilePath, Compression)
uncompressedName fp
          | isSuffixOf ".gz"  fp = (reverse . (drop 3) . reverse $ fp, GZ)
          | isSuffixOf ".bz2" fp = (reverse . (drop 4) . reverse $ fp, BZ2)
          | otherwise            = (fp, Uncompressed)

indexesInRelease :: (FilePath -> Bool)
                 -> Control -- ^ A release file
                 -> [(CheckSums, Integer, FilePath)] -- ^ 
indexesInRelease filterp (Control [p]) =
    let md5sums =
            case md5sumField p of
              (Just md5) -> md5
              Nothing -> error $ "Did not find MD5Sum field."
    in
      filter (\(_,_,fp) -> filterp fp) $ map (makeTuple . B.words) $ filter (not . B.null) (B.lines md5sums)
    where
      makeTuple :: [B.ByteString] -> (CheckSums, Integer, FilePath)
      makeTuple [md5sum, size, fp] = (CheckSums { md5sum = Just (B.unpack md5sum), sha1 = Nothing, sha256 = Nothing }, read (B.unpack size), B.unpack fp)
      makeTuple x = error $ "Invalid line in release file: " ++ show x
indexesInRelease _ x = error $ "Invalid release file: " ++ B.unpack (B.concat (formatControl x))

-- |make a FileTuple for a file found on the local disk
-- returns 'Nothing' if the file does not exist.
tupleFromFilePath :: FilePath -> FilePath -> IO (Maybe FileTuple)
tupleFromFilePath basePath fp =
          do e <- fileExist (basePath </> fp)
             if not e
              then return Nothing
              else do size <- getFileStatus (basePath </> fp) >>= return . fromIntegral . fileSize
                      md5 <- Misc.md5sum (basePath </> fp)
                      return $ Just (CheckSums { md5sum = Just md5, sha1 = Nothing, sha256 = Nothing }, size, fp)

-- |find the Contents-* files. These are not listed in the Release file
findContentsFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findContentsFiles filterP distDir =
          do files <- getDirectoryContents distDir
             return $ filter filterP $ filter (isPrefixOf "Contents-" . baseName) files
