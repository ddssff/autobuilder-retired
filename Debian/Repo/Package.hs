module Debian.Repo.Package
    ( -- * Source and binary packages 
      sourceFilePaths
    , binaryPackageSourceVersion
    , binarySourceVersion
    , sourcePackageBinaryNames
    , sourceBinaryNames
    , toSourcePackage
    , toBinaryPackage
    , binaryPackageSourceID
    , sourcePackageBinaryIDs
    , sourcePackagesOfIndex
    , sourcePackagesOfIndex'
    , binaryPackagesOfIndex
    , binaryPackagesOfIndex'
    , getPackages
    , putPackages
    , releaseSourcePackages
    , releaseBinaryPackages
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import Debian.Apt.Index (Compression(..), controlFromIndex)
import Debian.Control
import Debian.Repo.PackageIndex
import qualified Debian.Control.ByteString as B
import qualified Debian.Relation.ByteString as B
import Debian.Repo.IO
--import Debian.Shell
import Debian.Repo.Types
import Debian.URI
import Debian.Version

import Control.Exception (Exception(..))
import Control.Monad.Trans
import Control.Monad.State (get, put)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe
import qualified Extra.Either as EE
import qualified Extra.Files as EF
import Extra.CIO (CIO(..))
--import System.Directory
import System.FilePath((</>))
import System.IO.Unsafe
import System.Posix
--import System.Unix.Process
import Text.Regex

sourceFilePaths :: SourcePackage -> [FilePath]
sourceFilePaths package =
    map ((sourceDirectory package) </>) . map sourceFileName . sourcePackageFiles $ package

-- | Return the name and version number of the source package that
-- generated this binary package.  
binaryPackageSourceVersion :: BinaryPackage -> Maybe (String, DebianVersion)
binaryPackageSourceVersion package =
    let binaryName = packageName . packageID $ package
        binaryVersion = packageVersion . packageID $ package in
    binarySourceVersion' binaryName binaryVersion (packageInfo package)

-- |Return the name and version number of the source package that
-- generated this binary package.
-- see also: 'binaryPackageSourceVersion'
binarySourceVersion :: B.Paragraph -> Maybe ((String, DebianVersion), (String, DebianVersion))
binarySourceVersion paragraph =
    let mBinaryName = fmap B.unpack $ fieldValue "Package" paragraph
        mBinaryVersion = fmap (parseDebianVersion . B.unpack) $ fieldValue "Version" paragraph
    in
      case (mBinaryName, mBinaryVersion) of
        (Just binaryName, Just binaryVersion) ->
            fmap ((,) (binaryName, binaryVersion)) $ binarySourceVersion' binaryName binaryVersion paragraph
        _ -> Nothing

binarySourceVersion' :: (ControlFunctions a) => String -> DebianVersion -> Paragraph' a -> Maybe (String, DebianVersion)
binarySourceVersion' binaryName binaryVersion paragraph =
    case (B.fieldValue "Source" paragraph) of
      Just source ->
          case matchRegex re (asString source) of
            Just [name, _, ""] -> Just (name, binaryVersion)
            Just [name, _, version] -> Just (name, parseDebianVersion version)
            _ -> error "internal error"
      Nothing ->
          Just (asString binaryName, binaryVersion)
    where
      re = mkRegex "^[ ]*([^ (]*)[ ]*(\\([ ]*([^ )]*)\\))?[ ]*$"

sourcePackageBinaryNames :: SourcePackage -> [String]
sourcePackageBinaryNames package =
    sourceBinaryNames (sourceParagraph package)

sourceBinaryNames :: B.Paragraph -> [String]
sourceBinaryNames paragraph = 
    case B.fieldValue "Binary" paragraph of
      Just names -> splitRegex (mkRegex "[ ,]+") (B.unpack names)
      _ -> error ("Source package info has no 'Binary' field:\n" ++ (B.unpack . formatParagraph $ paragraph))

toSourcePackage :: PackageIndex -> B.Paragraph -> SourcePackage
toSourcePackage index package =
    case (B.fieldValue "Directory" package,
          B.fieldValue "Files" package,
          B.fieldValue "Package" package,
          maybe Nothing (Just . parseDebianVersion . B.unpack) (B.fieldValue "Version" package)) of
      (Just directory, Just files, Just name, Just version) ->
          case parseSourcesFileList files of
            Right files ->
                SourcePackage
                { sourcePackageID =
                      PackageID
                      { packageIndex = index
                      , packageName = B.unpack name
                      , packageVersion = version }
                , sourceParagraph = package
                , sourceDirectory = B.unpack directory
                , sourcePackageFiles = files }
            Left messages -> error $ "Invalid file list: " ++ show messages
      _ -> error $ "Missing info in source package control information:\n" ++ B.unpack (formatParagraph package)
    where      
      -- Parse the list of files in a paragraph of a Sources index.
      parseSourcesFileList :: B.ByteString -> Either [String] [SourceFileSpec]
      parseSourcesFileList text =
          merge . catMaybes . map parseSourcesFiles . lines . B.unpack $ text
      parseSourcesFiles line =
          case words line of
            [md5sum, size, name] -> Just (Right (SourceFileSpec md5sum (read size) name))
            [] -> Nothing
            _ -> Just (Left ("Invalid line in Files list: '" ++ show line ++ "'"))
      merge x = case partition (either (const True) (const False)) x of
                  (a, []) -> Left . catMaybes . map (either Just (const Nothing )) $ a
                  (_, a) -> Right . catMaybes . map (either (const Nothing) Just) $ a

toBinaryPackage :: PackageIndex -> B.Paragraph -> BinaryPackage
toBinaryPackage index p =
    case (B.fieldValue "Package" p, B.fieldValue "Version" p) of
      (Just name, Just version) ->
          BinaryPackage 
          { packageID = PackageID { packageIndex = index
                                  , packageName = B.unpack name
                                  , packageVersion = parseDebianVersion (B.unpack version) }
          , packageInfo = p
          , pDepends = tryParseRel $ B.lookupP "Depends" p
          , pPreDepends = tryParseRel $ B.lookupP "Pre-Depends" p
          , pConflicts = tryParseRel $ B.lookupP "Conflicts" p
          , pReplaces =  tryParseRel $ B.lookupP "Replaces" p
          , pProvides =  tryParseRel $ B.lookupP "Provides" p
          }
      _ -> error ("Invalid data in source index:\n " ++ packageIndexPath index)

tryParseRel :: Maybe B.Field -> B.Relations
tryParseRel (Just (B.Field (_, relStr))) = either (error . show) id (B.parseRelations relStr)
tryParseRel _ = []

-- | Parse the /Source/ field of a binary package's control
-- information, this may specify a version number for the source
-- package if it differs from the version number of the binary
-- package.
binaryPackageSourceID :: BinaryPackage -> PackageID
binaryPackageSourceID package =
    case maybe Nothing (matchRegex re . B.unpack) (B.fieldValue "Source" (packageInfo package)) of
      Just [name, _, ""] -> PackageID { packageIndex = sourceIndex
                                      , packageName = name
                                      , packageVersion = packageVersion id }
      Just [name, _, version] -> PackageID { packageIndex = sourceIndex
                                           , packageName = name
                                           , packageVersion = parseDebianVersion version }
      _ -> error "Missing Source attribute in binary package info"
    where
      sourceIndex = PackageIndex release component Source
      (PackageIndex release component _) = packageIndex id
      id = packageID package
      re = mkRegex "^[ ]*([^ (]*)[ ]*(\\([ ]*([^ )]*)\\))?[ ]*$"

sourcePackageBinaryIDs :: Arch -> SourcePackage -> [PackageID]
sourcePackageBinaryIDs Source _ = error "invalid argument"
sourcePackageBinaryIDs arch package =
    case (B.fieldValue "Version" info, B.fieldValue "Binary" info) of
      (Just version, Just names) -> map (binaryID (parseDebianVersion (B.unpack version))) $ splitRegex (mkRegex "[ ,]+") (B.unpack names)
      _ -> error ("Source package info has no 'Binary' field:\n" ++ (B.unpack . formatParagraph $ info))
    where
      -- Note that this version number may be wrong - we need to
      -- look at the Source field of the binary package info.
      binaryID version name = PackageID { packageIndex = binaryIndex
                                        , packageName = name
                                        , packageVersion = version }
      sourceIndex = packageIndex (sourcePackageID package)
      binaryIndex = sourceIndex { packageIndexArch = arch }
      info = sourceParagraph package

-- | Get the contents of a package index
getPackages :: CIO m => PackageIndex -> m (Either Exception [BinaryPackage])
getPackages index =
    liftIO (fileFromURI (uri {uriPath = uriPath uri </> packageIndexPath index ++ ".gz"})) >>=
    return . either Left (\ s -> case controlFromIndex GZ (show uri) s of
                                   Left e -> Left (ErrorCall (show e))
                                   Right (B.Control control) -> Right $ map (toBinaryPackage index) control)
    where
      uri = repoURI repo
      release = packageIndexRelease index
      repo = releaseRepo release

-- | Get the contents of a package index
binaryPackagesOfIndex :: CIO m => PackageIndex -> m (Either Exception [BinaryPackage])
binaryPackagesOfIndex index =
    case packageIndexArch index of
      Source -> return (Right [])
      _ -> getPackages index -- >>= return . either Left (Right . map (toBinaryPackage index . packageInfo))

-- | Get the contents of a package index
sourcePackagesOfIndex :: CIO m => PackageIndex -> m (Either Exception [SourcePackage])
sourcePackagesOfIndex index =
    case packageIndexArch index of
      Source -> getPackages index >>= return . either Left (Right . map (toSourcePackage index . packageInfo))
      _ -> return (Right [])

-- FIXME: assuming the index is part of the cache 
sourcePackagesOfIndex' :: (AptCache a, CIO m) => a -> PackageIndex -> AptIOT m [SourcePackage]
sourcePackagesOfIndex' cache index =
    do state <- get
       let cached = lookupSourcePackages path state
       status <- liftIO $ getFileStatus path
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = map (toSourcePackage index) paragraphs 
                 put (insertSourcePackages path (status, packages) state)
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache index

indexCacheFile :: (AptCache a) => a -> PackageIndex -> FilePath
indexCacheFile apt index =
    case (aptArch apt, packageIndexArch index) of
      (Source, _) -> error "Invalid build architecture: Source"
      (Binary _, Source) -> indexPrefix index ++ "_source_Sources"
      (Binary _, Binary arch) -> indexPrefix index ++ "_binary-" ++ arch ++ "_Packages"

indexPrefix :: PackageIndex -> FilePath
indexPrefix index =
    (escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText +?+ "dists_") ++
     releaseName' distro ++ "_" ++ (sectionName' $ section))
    where
      release = packageIndexRelease index
      section = packageIndexComponent index
      repo = releaseRepo release
      uri = repoURI repo
      distro = releaseInfoName . releaseInfo $ release
      scheme = uriScheme uri
      auth = uriAuthority uri
      path = uriPath uri
      userpass = maybe "" uriUserInfo auth
      reg = maybeOfString $ maybe "" uriRegName auth
      port = maybe "" uriPort auth
      (user, pass) = break (== ':') userpass
      user' = maybeOfString user
      pass' = maybeOfString pass
      uriText = prefix scheme user' pass' reg port path
      -- If user is given and password is not, the user name is
      -- added to the file name.  Otherwise it is not.  Really.
      prefix "http:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "http:" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix "ftp:" _ _ (Just host) _ path =
          host ++ escape path
      prefix "file:" Nothing Nothing Nothing "" path =
          escape path
      prefix "ssh:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "ssh" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix _ _ _ _ _ _ = error ("invalid repo URI: " ++ (uriToString' . repoURI. releaseRepo . packageIndexRelease $ index))
      maybeOfString "" = Nothing
      maybeOfString s = Just s
      escape s = intercalate "_" (wordsBy (== '/') s)
      wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
      wordsBy p s = 
          case (break p s) of
            (s, []) -> [s]
            (h, t) -> h : wordsBy p (drop 1 t)

(+?+) :: String -> String -> String
(+?+) a ('_' : b) = a +?+ b
(+?+) "" b = b
(+?+) a b =
    case last a of
      '_' -> (init a) +?+ b
      _ -> a ++ "_" ++ b

-- FIXME: assuming the index is part of the cache 
binaryPackagesOfIndex' :: (AptCache a, CIO m) => a -> PackageIndex -> AptIOT m [BinaryPackage]
binaryPackagesOfIndex' cache index =
    do state <- get
       let cached = lookupBinaryPackages path state
       status <- liftIO $ getFileStatus path
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = map (toBinaryPackage index) paragraphs 
                 put (insertBinaryPackages path (status, packages) state)
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache index

-- | Return a list of all source packages.
releaseSourcePackages :: CIO m => Release -> m (Either Exception [SourcePackage])
releaseSourcePackages release =
    mapM sourcePackagesOfIndex (sourceIndexList release) >>= return . test
    where
      test xs = case EE.partitionEithers xs of
                  ([], ok) -> Right (concat ok)
                  (bad, _) -> Left . ErrorCall $ intercalate ", " (map show bad)

-- | Return a list of all the binary packages for all supported architectures.
releaseBinaryPackages :: CIO m => Release -> m (Either Exception [BinaryPackage])
releaseBinaryPackages release =
    mapM binaryPackagesOfIndex (binaryIndexList release) >>= return . test
    where
      test xs = case EE.partitionEithers xs of
                  ([], ok) -> Right (concat ok)
                  (bad, _) -> Left . ErrorCall $ intercalate ", " (map show bad)

-- | Write a set of packages into a package index.
putPackages :: PackageIndexLocal ->  [BinaryPackageLocal] -> IO (Either [String] ())
putPackages index packages =
    case releaseRepo release of
      LocalRepo repo -> EF.writeAndZipFileWithBackup (outsidePath (repoRoot repo) </> packageIndexPath index) text
      x -> error $ "Package.putPackages: Expected local repository, found " ++ show x
    where
      release = packageIndexRelease index
      --repo = releaseRepo release
      text = L.fromChunks [B.concat (intersperse (B.pack "\n") . map formatParagraph . map packageInfo $ packages)]
