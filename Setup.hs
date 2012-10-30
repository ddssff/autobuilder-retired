#!/usr/bin/runhaskell

import Control.Monad (when)
import Debian.Relation (BinPkgName(..), PkgName(..))
import Distribution.Debian (debianize, Flags(..), Executable(..), defaultFlags)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import System.Cmd
import System.Exit

main = defaultMainWithHooks simpleUserHooks {
         postConf = \ _ _ _ lbi -> case buildDir lbi of
                                     "dist-debianize/build" -> debianize (flags lbi)
                                     "dist/build" -> debianize ((flags lbi) {dryRun = True})
       , postBuild = \ _ _ _ _ -> runTestScript
       , runTests = \ _ _ _ _ -> runTestScript
       }

flags lbi =
    defaultFlags { executablePackages = [Script "autobuilder" "autobuilder"]
                 , binaryPackageDeps = map mkdep [ "libghc-autobuilder-dev (= ${Source-Version})"
                                                 , "debootstrap"
                                                 , "rsync"
                                                 , "dupload"
                                                 , "darcs"
                                                 , "tla"
                                                 , "mercurial"
                                                 , "subversion"
                                                 , "apt"
                                                 , "build-essential"
                                                 , "quilt"
                                                 , "curl"
                                                 , "cabal-debian (>= 2)" ] }
    where
      mkdep p = (BinPkgName (PkgName "autobuilder"), BinPkgName (PkgName p))

runTestScript =
    system "runhaskell -isrc Test/Test.hs" >>= \ code ->
    if code == ExitSuccess then return () else error "Test Failure"
