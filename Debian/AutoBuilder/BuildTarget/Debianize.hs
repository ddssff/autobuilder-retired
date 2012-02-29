{-# LANGUAGE ScopedTypeVariables #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize (Debianize(..), prepare, documentation,
                                                 Hackage(..), prepareHackage, documentationHackage) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Z
-- import Control.Applicative.Error (maybeRead)
import Control.Exception (SomeException, try, throw)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
-- import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (isPrefixOf, isSuffixOf, intercalate, nub, sort)
import Data.Maybe (catMaybes)
import Data.Version (Version, showVersion, parseVersion)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Types.PackageFlag as P
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Repo hiding (getVersion)
import System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import System.Exit
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Unix.Directory (removeRecursiveSafely)
import System.Unix.Process (collectOutput, collectOutputUnpacked)
import System.Unix.Progress (lazyCommandE, lazyProcessE)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.XML.HaXml (htmlprint)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Html.Parse (htmlParse)
import Text.XML.HaXml.Posn

data Debianize = Debianize String (Maybe Version) SourceTree R.RetrieveMethod

documentation = [ "debianize:<name> or debianize:<name>=<version> - a target of this form"
                , "(currently) retrieves source code from http://hackage.haskell.org and runs"
                , "cabal-debian to create the debianization." ]

instance Download Debianize where
    method (Debianize _ _ _ m) = m
    getTop _ (Debianize _ _ tree _) = topdir tree
    revision _ (Debianize name (Just version) _ _) =
        return $ "debianize:" ++ name ++ "=" ++ showVersion version
    revision _ (Debianize _ Nothing _ _) =
        fail "Attempt to generate revision string for unversioned hackage target"
    logText (Debianize _ _ _ _) revision =
        "Built from hackage, revision: " ++ either show id revision
    mVersion (Debianize _ v _ _) = {- fmap (parseDebianVersion . showVersion) -} v

prepare :: P.CacheRec -> [P.PackageFlag] -> String -> [P.CabalFlag] -> R.RetrieveMethod -> AptIOT IO Debianize
prepare cache flags name cabalFlags m = liftIO $
    do (version' :: Version) <- maybe (getVersion (P.hackageServer (P.params cache)) name) (return . readVersion) versionString
       when (P.flushSource (P.params cache)) (removeRecursiveSafely (tarball (P.topDir cache) name version'))
       downloadAndDebianize cache cabalFlags flags name version'
       tree <- findSourceTree (unpacked (P.topDir cache) name version')
       return $ Debianize name (Just version') tree m
    where
      versionString = case nub (sort (catMaybes (map (\ flag -> case flag of
                                                                  P.CabalPin s -> Just s
                                                                  _ -> Nothing) cabalFlags))) of
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
downloadAndDebianize :: P.CacheRec -> [P.CabalFlag] -> [P.PackageFlag] -> String -> Version -> IO ()
downloadAndDebianize cache cabalFlags flags name version =
    removeRecursiveSafely (unpacked (P.topDir cache) name version) >>
    downloadCached (P.hackageServer (P.params cache)) (P.topDir cache) name version >>=
    unpack (P.topDir cache) >>
    patch (P.topDir cache) flags name version >>
    debianize cache cabalFlags flags (unpacked (P.topDir cache) name version)

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
         True -> let path = tarball top name version in
                 try (B.readFile path >>=
                      return . validate >>=
                      maybe (download' server top name version) return) >>=
                 either (\ (e :: SomeException) ->
                             let msg = "Failure reading " ++ path ++ ": " ++ show e in
                             hPutStrLn stderr msg >>
                             hPutStrLn stderr ("Removing " ++ path) >>
                             removeFile path >>
                             download' server top name version)
                        return
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
debianize :: P.CacheRec -> [P.CabalFlag] -> [P.PackageFlag] -> FilePath -> IO ()
debianize cache cflags pflags dir =
    do let pflags' = if any isMaintainerFlag pflags then pflags else P.Maintainer "Unknown Maintainer <unknown@debian.org>" : pflags
           args = (["--debianize"] ++
                   maybe [] (\ x -> ["--ghc-version", x]) ver ++
                   concatMap cflag cflags ++
                   concatMap pflag pflags')
       (out, err, code) <- lazyProcessE "cabal-debian" args (Just dir) Nothing B.empty >>= return . collectOutputUnpacked
       case code of
         ExitFailure n -> error ("cd " ++ show dir ++ " && cabal-debian " ++ intercalate " " args ++ "\n -> " ++ show n ++
                                 "\nStdout:\n" ++ out ++ "\nStderr:\n" ++ err)
         ExitSuccess -> return ()
    where
      cflag (P.ExtraDep s) = ["--build-dep", s]
      cflag (P.ExtraDevDep s) = ["--dev-dep", s]
      cflag (P.MapDep c d) = ["--map-dep", c ++ "=" ++ d]
      cflag (P.DebVersion s) = ["--deb-version", s]
      cflag (P.Revision s) = ["--revision", s]
      cflag (P.Epoch name d) = ["--epoch-map", name ++ "=" ++ show d]
      cflag _ = []
      pflag (P.Maintainer s) = ["--maintainer", s]
      pflag _ = []

      -- root = rootPath (P.cleanRootOfRelease cache (P.buildRelease (P.params cache)))
      ver = P.ghcVersion (P.params cache)
      isMaintainerFlag (P.Maintainer _) = True
      isMaintainerFlag _ = False

-- |Given a package name, get the newest version in hackage of the hackage package with that name:
-- > getVersion \"binary\" -> \"0.5.0.2\"
getVersion :: String -> String -> IO Version
getVersion server name =
    lazyCommandE cmd B.empty >>= return . readVersion . findVersion name . parse cmd
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
versionURL server name version = "http://" ++ server ++ "/packages/archive/" ++ name ++ "/" ++ showVersion version ++ "/" ++ name ++ "-" ++ showVersion version ++ ".tar.gz"

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

data Hackage = Hackage String (Maybe Version) SourceTree R.RetrieveMethod

documentationHackage = [ "hackage:<name> or hackage:<name>=<version> - a target of this form"
                , "retrieves source code from http://hackage.haskell.org." ]

instance Download Hackage where
    method (Hackage _ _ _ m) = m
    getTop _ (Hackage _ _ tree _) = topdir tree
    revision _ (Hackage name (Just version) _ _) =
        return $ "hackage:" ++ name ++ "=" ++ showVersion version
    revision _ (Hackage _ Nothing _ _) =
        fail "Attempt to generate revision string for unversioned hackage target"
    logText (Hackage _ _ _ _) revision =
        "Built from hackage, revision: " ++ either show id revision
    mVersion (Hackage _ v _ _) = {- fmap (parseDebianVersion . showVersion) -} v

prepareHackage :: P.CacheRec -> String -> [P.CabalFlag] -> R.RetrieveMethod -> AptIOT IO Hackage
prepareHackage cache name cabalFlags m = liftIO $
    do (version' :: Version) <- maybe (getVersion (P.hackageServer (P.params cache)) name) (return . readVersion) versionString
       when (P.flushSource (P.params cache)) (mapM_ removeRecursiveSafely [tarball (P.topDir cache) name version', unpacked (P.topDir cache) name version'])
       downloadCached (P.hackageServer (P.params cache)) (P.topDir cache) name version' >>= unpack (P.topDir cache)
       tree <- findSourceTree (unpacked (P.topDir cache) name version')
       return $ Hackage name (Just version') tree m
    where
      versionString = case nub (sort (catMaybes (map (\ flag -> case flag of
                                                                  P.CabalPin s -> Just s
                                                                  _ -> Nothing) cabalFlags))) of
                        [] -> Nothing
                        [v] -> Just v
                        vs -> error ("Conflicting cabal version numbers passed to Debianize: [" ++ intercalate ", " vs ++ "]")

readVersion :: String -> Version
readVersion s =
    case filter (null . snd) $ readP_to_S parseVersion s of
      [(v, _)] -> v
      _ -> error $ "Failure reading cabal vesion: " ++ show s
