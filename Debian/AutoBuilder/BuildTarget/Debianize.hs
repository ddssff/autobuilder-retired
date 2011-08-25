{-# LANGUAGE ScopedTypeVariables #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize (Debianize(..), prepare, documentation) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Z
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.List (isPrefixOf, isSuffixOf, intercalate)
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Params as P
import Debian.Version (DebianVersion, parseDebianVersion)
import Debian.Repo hiding (getVersion)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Unix.Directory (removeRecursiveSafely)
import System.Unix.Process (collectOutput, collectOutputUnpacked, lazyProcess)
import System.Unix.Progress (lazyCommandE)
import Text.XML.HaXml (htmlprint)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Html.Parse (htmlParse)
import Text.XML.HaXml.Posn

data Debianize = Debianize String (Maybe DebianVersion) SourceTree

instance Show Debianize where
    show (Debianize name version _) = "debianize:" ++ name ++ maybe "" (("=" ++) . show) version

documentation = [ "debianize:<name> or debianize:<name>=<version> - a target of this form"
                , "(currently) retrieves source code from http://hackage.haskell.org and runs"
                , "cabal-debian to create the debianization." ]

instance BuildTarget Debianize where
    getTop _ (Debianize _ _ tree) = topdir tree
    revision _ (Debianize name (Just version) _) =
        return $ "debianize:" ++ name ++ "=" ++ show version
    revision _ (Debianize _ Nothing _) =
        fail "Attempt to generate revision string for unversioned hackage target"
    logText (Debianize _ _ _) revision =
        "Built from hackage, revision: " ++ either show id revision
    mVersion (Debianize _ v _) = v

prepare :: P.CacheRec -> [P.PackageFlag] -> String -> Maybe String -> AptIOT IO Debianize
prepare cache flags name version = liftIO $
    do (version' :: DebianVersion) <- maybe (getVersion name) (return . parseDebianVersion) version
       when (P.flushSource (P.params cache)) (mapM_ removeRecursiveSafely [destPath top name version', destDir top name version'])
       downloadAndDebianize cache flags name version' >>= findSourceTree >>= return . Debianize name (Just version')
    where
      top = P.topDir cache

parse cmd output =
    case collectOutputUnpacked output of
      (out, _, ExitSuccess) -> htmlParse cmd out
      (_, _, _) -> error (cmd ++ " -> " ++ show output)

-- |Download and unpack the given package version to the autobuilder's
-- hackage temporary directory:
-- > download \"/home/dsf/.autobuilder/hackage\" -> \"/home/dsf/.autobuilder/hackage/happstack-server-6.1.4.tar.gz\"
-- After the download it tries to untar the file, and then it saves the compressed tarball.
downloadAndDebianize ::  P.CacheRec -> [P.PackageFlag] -> String -> DebianVersion -> IO String
downloadAndDebianize cache flags name version =
    do let dest = destPath top name version
       exists <- doesFileExist dest
       dir <-
           case exists of
             True -> 
                 do text <- B.readFile dest
                    let entries = Tar.read (Z.decompress text)
                    case Tar.foldEntries (\ _ -> either error (Right . (+ 1))) (Right 0) Left entries of
                      Left _ -> download top name version
                      Right _ -> return (destDir top name version)
             False -> download top name version
       debianize cache flags dir
       return dir
    where
      top = P.topDir cache

-- |FIXME: It would be better to have a cabal: target which did this
-- debianization, then we could debianize cabal packages whatever their origin,
-- and we wouldn't have to debianize *every* hackage target.  But I'm out of
-- patience right now...
debianize :: P.CacheRec -> [P.PackageFlag] -> FilePath -> IO ()
debianize cache flags dir =
    do let flags' = (["--debianize", "--maintainer", "Unknown Maintainer <unknown@debian.org>"] ++
                     foldr (\ flag args ->
                                case flag of
                                  (P.ExtraDep s) -> ["--build-dep", s] ++ args
                                  (P.DebVersion s) -> ["--deb-version", s] ++ args
                                  (P.Epoch n) -> ["--epoch", show n] ++ args
                                  _ -> args) [] flags ++
                               [{- "--root", root -}] ++
                               -- Used to determine which packages are bundled
                               maybe [] (\ x -> ["--ghc-version", x]) ver)
       -- hPutStrLn stderr ("cabal-debian " ++ intercalate " " flags')
       (out, err, code) <- lazyProcess "cabal-debian" flags' (Just dir) Nothing B.empty >>= return . collectOutputUnpacked
       case code of
         ExitFailure n -> error ("cd " ++ show dir ++ " && cabal-debian --debianize --maintainer 'Unknown Maintainer <unknown@debian.org>' --root " ++ show root ++ "\n -> " ++ show n ++ "\nStdout:\n" ++ out ++ "\nStderr:\n" ++ err)
         ExitSuccess -> return ()
    where
      root = rootPath (P.cleanRootOfRelease cache (P.buildRelease (P.params cache)))
      ver = P.ghcVersion (P.params cache)

-- |Download without checking whether the file was already downloaded.
download :: String -> String -> DebianVersion -> IO String
download top name version =
    let dest = destPath top name version in
    lazyCommandE (downloadCommand top name version) B.empty >>=
    return . collectOutput >>= \ (out, err, res) ->
    case (err, res) of
      (_, ExitFailure _) ->
          let msg = downloadCommand top name version ++ " ->\n" ++ show (err, res) in
          hPutStrLn stderr msg >>
          error msg
      (_, ExitSuccess) ->
          do Tar.unpack (tmpDir top) (Tar.read (Z.decompress out))
             createDirectoryIfMissing True (tmpDir top)
             B.writeFile dest out
             return (destDir top name version)

downloadCommand _top name version = "curl -s '" ++ versionURL name version ++ "'" {- ++ " > '" ++ destPath top name version ++ "'" -}
destPath top name version = destDir top name version ++ ".tar.gz"
destDir top name version = tmpDir top ++ "/" ++ name ++ "-" ++ show version
tmpDir top = top ++ "/hackage"

-- |Given a package name, get the newest version in hackage of the hackage package with that name:
-- > getVersion \"binary\" -> \"0.5.0.2\"
getVersion :: String -> IO DebianVersion
getVersion name =
    lazyCommandE cmd B.empty >>= return . parseDebianVersion . findVersion name . parse cmd
    where cmd = curlCmd (packageURL name)

curlCmd url = "curl -s '" ++ url ++ "'"

findVersion :: String -> Document Posn -> String
findVersion package (Document _ _ (Elem _name _attrs content) _) =
    case doContentList content of
      [s] -> s
      _ss -> error ("Could not find version number of " ++ package ++ " in " ++ show (map (htmlprint . (: [])) content))
    where
      doContentList [CElem (Elem "head" _ _) _, CElem (Elem "body" _ content) _] = doContentList content
      doContentList [CElem (Elem "div" _ _) _, CElem (Elem "div" _ content) _, CElem (Elem "div" _ _) _] = doContentList content
      doContentList (CElem (Elem "h1" _ _) _ : etc) = doContentList (drop (length etc - 2) etc)
      doContentList [CElem (Elem "h2" _ _) _, CElem (Elem "ul" _ content) _] = doContentList content
      doContentList [CElem (Elem "li" _ content) _, _] = doContentList content
      doContentList [CElem (Elem "a" _ content) _, _] = doContentList content
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
packageURL name = "http://hackage.haskell.org/package/" ++ name
versionURL name version = "http://hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ show version ++ "/" ++ name ++ "-" ++ show version ++ ".tar.gz"

