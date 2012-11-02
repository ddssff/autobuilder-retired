{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -Werror #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize
    ( prepare
    , documentation
    ) where

import Control.Exception (SomeException, catch)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (isSuffixOf)
import Data.Maybe (mapMaybe)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo hiding (getVersion, pkgName, pkgVersion)
import Distribution.Verbosity (normal)
import Distribution.Package (PackageIdentifier(..) {-, PackageName(..)-})
import Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
import Distribution.PackageDescription.Parse (readPackageDescription)
import Prelude hiding (catch)
import System.Directory (getDirectoryContents, doesFileExist)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Process (CreateProcess(cwd, env), CmdSpec(RawCommand))
import System.Process.Read.Monad (runProcessF)
import System.Process.Progress (qPutStrLn)
import System.Unix.Directory (removeRecursiveSafely)
--import System.Unix.QIO (qPutStrLn)

documentation :: [String]
documentation = [ "hackage:<name> or hackage:<name>=<version> - a target of this form"
                , "retrieves source code from http://hackage.haskell.org." ]

-- | Debianize the download, which is assumed to be a cabal package.
prepare :: MonadApt e m => P.CacheRec -> P.Packages -> T.Download -> m T.Download
prepare cache package' cabal = liftIO $
    getDirectoryContents (T.getTop cabal) >>= return . filter (isSuffixOf ".cabal") >>= \ cabfiles ->
    case cabfiles of
      [cabfile] ->
          do desc <- readPackageDescription normal (T.getTop cabal </> cabfile)
             let version = pkgVersion . package . packageDescription $ desc
             -- removeRecursiveSafely (T.getTop cabal </> "debian")
             debianize cache (P.flags package') (T.getTop cabal)
             return $ T.Download { T.package = package'
                                 , T.getTop = T.getTop cabal
                                 , T.logText =  "Built from hackage, revision: " ++ show (P.spec package')
                                 , T.mVersion = Just version
                                 , T.origTarball = T.origTarball cabal
                                 , T.cleanTarget = \ top -> T.cleanTarget cabal top
                                 , T.buildWrapper = id }
      _ -> error $ "Download at " ++ T.getTop cabal ++ " missing or multiple cabal files"

-- | There are two ways to debianize a package.  The old way is to run
-- cabal-debian.  The new way is to run Setup configure
-- --builddir=dist-debianize.  This second only works if support has
-- been added to the setup to run the debianize function of the
-- cabal-debian library, so if it doesn't work the old method is used.
debianize :: P.CacheRec -> [P.PackageFlag] -> FilePath -> IO ()
debianize cache pflags dir =
    qPutStrLn ("debianizing " ++ dir) >> runSetupConfigure >>= runCabalDebian
    where
      runSetupConfigure :: IO Bool
      runSetupConfigure =
          (do removeRecursiveSafely (dir </> "debian/compat")
              let flags :: [String]
                  flags = concat (mapMaybe cdflags pflags)
                  cdflags (P.CabalDebian s) = Just s
                  cdflags _ = Nothing
              oldEnv <- getEnvironment >>= return . filter (not . (== "CABALDEBIAN") . fst)
              let newEnv = ("CABALDEBIAN", show flags) : oldEnv
              _ <- runProcessF (\ p -> p {cwd = Just dir, env = Just newEnv}) (RawCommand "runhaskell" ["Setup", "configure", "--builddir=debian"]) B.empty
              doesFileExist (dir </> "debian/compat")) `catch` (\ (_ :: SomeException) -> return False)

      runCabalDebian True = return ()
      runCabalDebian False =
          (do _ <- runProcessF (\ p -> p {cwd = Just dir}) (RawCommand "cabal-debian" args) B.empty
              return ()) `catch` (\ (e :: SomeException) -> error (show e))
          where
            args = (["--debianize"] ++ maybe [] (\ x -> ["--ghc-version", x]) ver ++ concatMap pflag pflags)
            pflag (P.CabalDebian ss) = ss
            pflag _ = []

            ver = P.ghcVersion (P.params cache)
{-
            indent pre s = unlines $ map (pre ++) $ lines $ B.unpack $ s
            isMaintainerFlag (P.CabalDebian xs) | elem "--maintainer" xs = True
            isMaintainerFlag _ = False

            run cmd args cwd input =
                hPutStrLn stderr ("-> " ++ showCommandForUser cmd args ++ " (in " ++ show dir ++ ")") >>
                readModifiedProcessWithExitCode cwd (RawCommand cmd args) input
-}
