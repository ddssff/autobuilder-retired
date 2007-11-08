module Main where

import Debian.Types
import Debian.Index
import Debian.Package
import Debian.Local.Repo
import Debian.Local.Release
import Debian.Local.Index
import Debian.Local.Package
import Debian.Local.Insert
import Data.List
--import Repository
import qualified System.IO as IO
import Linspire.Unix.Directory
import System.Directory
import Debian.IO

top = "/tmp/testrepo"

eputStr s = IO.hPutStr IO.stderr s

main :: IO ()
main =
    do
      removeRecursiveSafely top
      createDirectoryIfMissing True top
      repo <- run (defStyle 0) repoTests
      releases <- run (defStyle 0) $ releaseTests repo
      run (defStyle 0) $ uploadTests releases
      return ()

repoTests :: AptIO LocalRepo
repoTests =
    do
      vPutStr 0 "------------- Repo Tests -------------"
      vPutStr 0 (" -> prepareRepository False \"" ++ top ++ "\" (Just Pool)")
      -- This fails when it tries to stat nonexistant directories
      -- prepareRepository True top (Just Pool)
      repo <- prepareLocalRepository (EnvPath (EnvRoot "") top) (Just Pool)
      vPutStr 0 (" <- " ++ show repo)
      return repo

releaseTests :: LocalRepo -> AptIO [Release LocalRepo]
releaseTests repo =
    do
      vPutStr 0 "------------- Release Tests -------------"
      let dist = "testrelease"
      let aliases = ["skipjack-feisty"]
      let components = map Section ["main", "contrib", "non-free"]
      let archList = [Binary "i386", Binary "amd64"]
      vPutStr 0 (" -> prepareRelease repo (ReleaseName " ++ show dist ++ ") (map ReleaseName " ++ show aliases ++ ") " ++ show components ++ " " ++ show archList)
      -- prepareRelease True repo (ReleaseName dist) (map ReleaseName aliases) components archList
      release <- prepareRelease repo (ReleaseName dist) (map ReleaseName aliases) components archList
      vPutStr 0 (" <- " ++ show release)
      let repo' = releaseRepo release
      releases <- findReleases repo'
      vPutStr 0 "All releases:"
      mapM_ (vPutStr 0) (map (("  " ++) . show) releases)
      return releases

dir = "/home/david/.autobuilder/localpools/CNRUbuntu/"
files = ["freespire-build-specs_0.1-0r1cnr13_all.deb",
         "freespire-build-specs_0.1-0r1cnr13_amd64.changes",
         "freespire-build-specs_0.1-0r1cnr13.dsc",
         "freespire-build-specs_0.1-0r1cnr13.tar.gz"]

uploadTests :: [Release LocalRepo] -> AptIO ()
uploadTests [release]  =
    do
      vPutStr 0 "------------- Upload Tests -------------"
      let repo = releaseRepo release
      io $ mapM_ upload files
      vPutStr 0 " -> dryRun $ scanIncoming repo' (Component \"main\")"
      withStyle (dryRun $ defStyle 0) $ scanIncoming True Nothing repo
      vPutStr 0 " -> realRun $ scanIncoming repo (Component \"main\")"
      (_, errors) <- scanIncoming True Nothing repo
      case errors of
        [] -> vPutStr 0 " <- Ok"
        _ -> vPutStr 0 (" <- " ++ show errors)
      -- get some package info
      let (sourceIndexes :: [PackageIndexLocal]) =
              filter (\ index -> packageIndexComponent index == Section "main") (sourceIndexList release)
      vPutStr 0 ("sourceIndexes: " ++ show sourceIndexes)
      (packages :: [BinaryPackageLocal]) <- io $ getPackages (head sourceIndexes)
      vPutStr 0 (" -> getPackages " ++ show (head sourceIndexes))
      vPutStr 0 (" <- " ++ show (map (show . packageID) packages))
      -- delete a source package
      -- Delete a source package that has a binary with a different version number
      return ()
    where
      upload file = readFile (dir ++ file) >>= writeFile (top ++ "/incoming/" ++ file)
uploadTests releases = error ("Too many releases: " ++ show releases)
