{-# LANGUAGE ScopedTypeVariables #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize (Debianize(..), prepare, documentation,
                                                 Hackage(..), prepareHackage, documentationHackage) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Z
import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
-- import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (isPrefixOf, isSuffixOf, intercalate, nub, sort)
import Data.Maybe (catMaybes)
import Data.Version (Version, showVersion)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Types.PackageFlag as P
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Version (parseDebianVersion)
import Debian.Repo hiding (getVersion)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Exit
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Unix.Directory (removeRecursiveSafely)
import System.Unix.Process (collectOutput, collectOutputUnpacked)
import System.Unix.Progress (lazyCommandE, lazyProcessE)
import Text.XML.HaXml (htmlprint)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Html.Parse (htmlParse)
import Text.XML.HaXml.Posn

data Debianize = Debianize String (Maybe Version) SourceTree

instance Show Debianize where
    show (Debianize name version _) = "debianize:" ++ name ++ maybe "" (("=" ++) . showVersion) version

documentation = [ "debianize:<name> or debianize:<name>=<version> - a target of this form"
                , "(currently) retrieves source code from http://hackage.haskell.org and runs"
                , "cabal-debian to create the debianization." ]

instance BuildTarget Debianize where
    getTop _ (Debianize _ _ tree) = topdir tree
    revision _ (Debianize name (Just version) _) =
        return $ "debianize:" ++ name ++ "=" ++ showVersion version
    revision _ (Debianize _ Nothing _) =
        fail "Attempt to generate revision string for unversioned hackage target"
    logText (Debianize _ _ _) revision =
        "Built from hackage, revision: " ++ either show id revision
    mVersion (Debianize _ v _) = fmap (parseDebianVersion . showVersion) v

prepare :: P.CacheRec -> [P.PackageFlag] -> String -> [P.CabalFlag] -> AptIOT IO Debianize
prepare cache flags name cabalFlags = liftIO $
    do (version' :: Version) <- maybe (getVersion (P.hackageServer (P.params cache)) name) (return . read) versionString
       when (P.flushSource (P.params cache)) (removeRecursiveSafely (tarball (P.topDir cache) name version'))
       downloadAndDebianize cache flags name version'
       findSourceTree (unpacked (P.topDir cache) name version') >>= return . Debianize name (Just version')
    where
      versionString = case nub (sort (catMaybes (map (\ flag -> case flag of
                                                                  P.CabalPin s -> Just s
                                                                  {- _ -> Nothing -}) cabalFlags))) of
                        [] -> Nothing
                        [v] -> Just v
                        vs -> error ("Conflicting cabal version numbers passed to Debianize: [" ++ intercalate ", " vs ++ "]")

parse cmd output =
    case collectOutputUnpacked output of
      (out, _, ExitSuccess) -> htmlParse cmd out
      (_, _, _) -> error (cmd ++ " -> " ++ show output)

-- |Download and unpack the given package version to the autobuilder's
-- hackage temporary directory:
-- > download \"/home/dsf/.autobuilder/hackage\" -> \"/home/dsf/.autobuilder/hackage/happstack-server-6.1.4.tar.gz\"
-- After the download it tries to untar the file, and then it saves the compressed tarball.
downloadAndDebianize :: P.CacheRec -> [P.PackageFlag] -> String -> Version -> IO ()
downloadAndDebianize cache flags name version =
    removeRecursiveSafely (unpacked (P.topDir cache) name version) >>
    downloadCached (P.hackageServer (P.params cache)) (P.topDir cache) name version >>=
    unpack (P.topDir cache) >>
    patch (P.topDir cache) flags name version >>
    debianize cache flags (unpacked (P.topDir cache) name version)

-- |Scan the flag list for Patch flag, and apply the patches
patch :: FilePath -> [P.PackageFlag] -> String -> Version -> IO ()
patch top flags name version =
    mapM_ patch' flags
    where
      patch' :: P.PackageFlag -> IO ()
      patch' (P.Patch text) =
          do (_out, err, res) <- lazyProcessE "/usr/bin/patch" ["-p1"] (Just (unpacked top name version)) Nothing text >>=
                                 return . collectOutputUnpacked
             case res of
               ExitFailure n -> error ("patch " ++ show (unpacked top name version) ++ " -> " ++
                                       show n ++ "\noutput: " ++ err ++ "\npatch:\n" ++ B.unpack text)
               ExitSuccess -> return ()
      patch' _ = return ()

-- |Unpack and save the files of a tarball.
unpack :: FilePath -> B.ByteString -> IO ()
unpack top text = Tar.unpack (tmpDir top) (Tar.read (Z.decompress text))

-- |Download and unpack the given package version to the autobuilder's
-- hackage temporary directory.  After the download it validates the
-- tarball text and saves the compressed tarball.
downloadCached :: String -> FilePath -> String -> Version -> IO B.ByteString
downloadCached server top name version =
    do exists <- doesFileExist (tarball top name version)
       case exists of
         True -> B.readFile (tarball top name version) >>=
                 return . validate >>=
                 maybe (download' server top name version) return
         False -> download' server top name version
    
-- |Download and save the tarball, return its contents.
download' :: String -> FilePath -> String -> Version -> IO B.ByteString
download' server top name version =
    do (out, err, res) <- lazyCommandE (downloadCommand server name version) B.empty >>= return . collectOutput
       case res of
         ExitFailure _ ->
             let msg = downloadCommand server name version ++ " ->\n" ++ show (err, res) in
             hPutStrLn stderr msg >>
             error msg
         ExitSuccess ->
             do createDirectoryIfMissing True (tmpDir top)
                B.writeFile (tarball top name version) out
                return out

-- |FIXME: It would be better to have a cabal: target which did this
-- debianization, then we could debianize cabal packages whatever their origin,
-- and we wouldn't have to debianize *every* hackage target.  But I'm out of
-- patience right now...
debianize :: P.CacheRec -> [P.PackageFlag] -> FilePath -> IO ()
debianize cache flags dir =
    do let flags' = if any isMaintainerFlag flags then flags else P.Maintainer "Unknown Maintainer <unknown@debian.org>" : flags
           flags'' = (["--debianize"] ++
                      foldr (\ flag args ->
                             case flag of
                               (P.ExtraDep s) -> ["--build-dep", s] ++ args
                               (P.ExtraDevDep s) -> ["--dev-dep", s] ++ args
                               (P.MapDep c d) -> ["--map-dep", c ++ "=" ++ d] ++ args
                               (P.DebVersion s) -> ["--deb-version", s] ++ args
                               (P.Revision s) -> ["--revision", s] ++ args
                               (P.Epoch name d) -> ["--epoch-map", name ++ "=" ++ show d] ++ args
                               (P.Maintainer s) -> ["--maintainer", s] ++ args
                               _ -> args) [] flags' ++
                      [{- "--root", root -}] ++
                      -- Used to determine which packages are bundled
                      maybe [] (\ x -> ["--ghc-version", x]) ver)
       -- hPutStrLn stderr ("cabal-debian " ++ intercalate " " flags')
       (out, err, code) <- lazyProcessE "cabal-debian" flags'' (Just dir) Nothing B.empty >>= return . collectOutputUnpacked
       case code of
         ExitFailure n -> error ("cd " ++ show dir ++ " && cabal-debian " ++ intercalate " " flags'' ++ "\n -> " ++ show n ++
                                 "\nStdout:\n" ++ out ++ "\nStderr:\n" ++ err)
         ExitSuccess -> return ()
    where
      -- root = rootPath (P.cleanRootOfRelease cache (P.buildRelease (P.params cache)))
      ver = P.ghcVersion (P.params cache)
      isMaintainerFlag (P.Maintainer _) = True
      isMaintainerFlag _ = False

-- |Given a package name, get the newest version in hackage of the hackage package with that name:
-- > getVersion \"binary\" -> \"0.5.0.2\"
getVersion :: String -> String -> IO Version
getVersion server name =
    lazyCommandE cmd B.empty >>= return . read . findVersion name . parse cmd
    where cmd = curlCmd (packageURL server name)
          curlCmd url = "curl -s '" ++ url ++ "'"

findVersion :: String -> Document Posn -> String
findVersion package (Document _ _ (Elem _name _attrs content) _) =
    case doContentList content of
      [s] -> s
      _ss -> error ("Could not find version number of " ++ package ++ " in " ++ show (map (htmlprint . (: [])) content))
    where
      doContentList [CElem (Elem (N "head") _ _) _, CElem (Elem (N "body") _ content) _] = doContentList content
      doContentList [CElem (Elem (N "div") _ _) _, CElem (Elem (N "div") _ content) _, CElem (Elem (N "div") _ _) _] = doContentList content
      doContentList (CElem (Elem (N "h1") _ _) _ : etc) = doContentList (drop (length etc - 2) etc)
      doContentList [CElem (Elem (N "h2") _ _) _, CElem (Elem (N "ul") _ content) _] = doContentList content
      doContentList (CElem (Elem (N "li") _ content) _ :  _) = doContentList content
      doContentList (CElem (Elem (N "a") _ content) _ : _) = doContentList content
      doContentList [CString _ c _] = [parseTarballName c]
      doContentList xs = error (show (map ((: []) . htmlprint . (: [])) xs))
      parseTarballName s =
          let prefix = package ++ "-"
              suffix = ".tar.gz" in
          if isPrefixOf prefix s && isSuffixOf suffix s
          then let s' = drop (length prefix) s in
               take (length s' - length suffix) s'
          else error $ "findVersion - not a tarball: " ++ show s

-- |Hackage paths
packageURL server name = "http://" ++ server ++ "/package/" ++ name
versionURL server name version = "http://" ++ server ++ "/packages/archive/" ++ name ++ "/" ++ show (showVersion version) ++ "/" ++ name ++ "-" ++ show (showVersion version) ++ ".tar.gz"

-- |Validate the text of a tarball file.
validate :: B.ByteString -> Maybe B.ByteString
validate text =
    let entries = Tar.read (Z.decompress text) in
    case Tar.foldEntries (\ _ -> either throw (Right . (+ 1))) (Right 0) Left entries of
      Left _ -> Nothing
      Right _ -> Just text

tarball :: FilePath -> String -> Version -> FilePath
tarball top name version  = tmpDir top </> name ++ "-" ++ showVersion version ++ ".tar.gz"

unpacked :: FilePath -> String -> Version -> FilePath
unpacked top name version = tmpDir top </> name ++ "-" ++ showVersion version

tmpDir :: FilePath -> FilePath
tmpDir top = top ++ "/hackage"

downloadCommand :: String -> String -> Version -> String
downloadCommand server name version = "curl -s '" ++ versionURL server name version ++ "'" {- ++ " > '" ++ destPath top name version ++ "'" -}

-- Hackage target

data Hackage = Hackage String (Maybe Version) SourceTree

instance Show Hackage where
    show (Hackage name version _) = "hackage:" ++ name ++ maybe "" (("=" ++) . showVersion) version

documentationHackage = [ "hackage:<name> or hackage:<name>=<version> - a target of this form"
                , "retrieves source code from http://hackage.haskell.org." ]

instance BuildTarget Hackage where
    getTop _ (Hackage _ _ tree) = topdir tree
    revision _ (Hackage name (Just version) _) =
        return $ "hackage:" ++ name ++ "=" ++ showVersion version
    revision _ (Hackage _ Nothing _) =
        fail "Attempt to generate revision string for unversioned hackage target"
    logText (Hackage _ _ _) revision =
        "Built from hackage, revision: " ++ either show id revision
    mVersion (Hackage _ v _) = fmap (parseDebianVersion . showVersion) v

prepareHackage :: P.CacheRec -> String -> [P.CabalFlag] -> AptIOT IO Hackage
prepareHackage cache name cabalFlags = liftIO $
    do (version' :: Version) <- maybe (getVersion (P.hackageServer (P.params cache)) name) (return . read) versionString
       when (P.flushSource (P.params cache)) (mapM_ removeRecursiveSafely [tarball (P.topDir cache) name version', unpacked (P.topDir cache) name version'])
       downloadCached (P.hackageServer (P.params cache)) (P.topDir cache) name version' >>= unpack (P.topDir cache)
       findSourceTree (unpacked (P.topDir cache) name version') >>= return . Hackage name (Just version')
    where
      versionString = case nub (sort (catMaybes (map (\ flag -> case flag of
                                                                  P.CabalPin s -> Just s
                                                                  {- _ -> Nothing -}) cabalFlags))) of
                        [] -> Nothing
                        [v] -> Just v
                        vs -> error ("Conflicting cabal version numbers passed to Debianize: [" ++ intercalate ", " vs ++ "]")
