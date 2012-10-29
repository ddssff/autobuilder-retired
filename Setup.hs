#!/usr/bin/runhaskell

import Control.Exception (SomeException)
import Distribution.Debian (debianize, Flags(..), Executable(..), defaultFlags)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import System.Cmd
import System.Exit

main = defaultMainWithHooks simpleUserHooks {
         postConf = \ _ _ _ lbi -> debianize (defaultFlags { executablePackages = [Script "autobuilder" "autobuilder"]
                                                           , binaryPackageDeps = map (\ p-> ("autobuilder",p))
                                                                                     [ "libghc-autobuilder-dev (= ${Source-Version})"
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
                                                                                     , "cabal-debian (>= 2)" ]
                                                           , debMaintainer = Just "David Fox <dsf@seereason.com>"
                                                           , dryRun = case buildDir lbi of "dist/build" -> True; "dist-ghc/build" -> False })
       , postBuild = \ _ _ _ _ -> runTestScript
       , runTests = \ _ _ _ _ -> runTestScript
       }

runTestScript =
    system "runhaskell -isrc Test/Test.hs" >>= \ code ->
    if code == ExitSuccess then return () else error "Test Failure"
