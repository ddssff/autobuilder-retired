#!/usr/bin/env runhaskell -package=base-3.0.3.0
import Data.List (isSuffixOf)
import Data.Maybe
import qualified Debian.AutoBuilder.Main as M
import qualified Debian.AutoBuilder.ParamClass as P
import Debian.AutoBuilder.ParamRec
import Debian.AutoBuilder.Params (defaultParams)
import Debian.GenBuildDeps (SrcPkgName(SrcPkgName), BinPkgName(BinPkgName), RelaxInfo(RelaxInfo))
import Debian.Repo.Cache (SourcesChangedAction(SourcesChangedError))
import Debian.Repo.Types (SliceName(SliceName, sliceName), ReleaseName(ReleaseName, relName), Arch(Binary))
import Debian.URI
import Debian.Version
import System.IO (hPutStrLn, hFlush, stderr)

main =
    hPutStrLn stderr "Autobuilder starting..." >> hFlush stderr >> M.main [params]
    -- getArgs >>= \ args -> M.main [doOptions params args]

-- The name of the upstream release that the the build release will be
-- based on.  This sources.list is combined with the one constructed
-- from the Build-URI to create the build environment.
myBaseRelease = "hardy" ++ if myBuildPrivateTargets then "-seereason" else ""

-- If True build the private targets, otherwise the public.
myBuildPrivateTargets = True

-- 
myUploadHost = "deb.seereason.com"
myVendorTag = "seereason"
myTargets = case myBuildPrivateTargets of
              False -> ghc610CoreTargets ++ autobuilderTargets ++ ghc610Targets ++ otherTargets
              True -> privateTargets
myGoals = []
myForceBuild = []
myVerbosity = 0
myUbuntuMirrorHost = "mirror.anl.gov"
myDebianMirrorHost = "mirror.anl.gov"

myBaseRepo = releaseRepoName myBaseRelease

myDoUpload = True
myDoNewDist = True

-- This URI is the address of the remote repository to which packages
-- will be uploaded after a run with no failures, when the myDoUpload
-- flag is true.  Packages are uploaded to the directory created by
-- appending '/incoming' to this URI.  This is distinct from the
-- local repository, where each packages is uploaded immediately after
-- it is built for use as build dependencies of other packages during
-- the same run.
myUploadURI = case myBuildPrivateTargets of
                False -> parseURI $ "ssh://upload@" ++ myUploadHost ++ "/srv/deb" ++ "/" ++ myBaseRepo
                True -> parseURI $ myPrivateBuildURI ++ "/" ++ myBaseRepo

-- An alternate url for the same repository the upload-uri points to,
-- used for downloading packages that have already been installed
-- there.
myBuildURI = case myBuildPrivateTargets of
               False -> parseURI $ "http://" ++ myUploadHost ++ "/" ++ myBaseRepo
               True -> parseURI $ myPrivateUploadURI ++ "/" ++ myBaseRepo

myPrivateUploadURI = "ssh://upload@deb.seereason.com/srv/deb-private"
myPrivateBuildURI = "ssh://upload@deb.seereason.com/srv/deb-private"
myPrivateDarcsURI = "ssh://upload@src.seereason.com/srv/darcs"

-- Additional packages to include in the clean build environment.
-- Adding packages here can speed things up when you are building many
-- packages, because for each package it reverts the build environment
-- to the clean environment and then installs all the build
-- dependencies.  This only affects newly created environments, so if
-- you change this value use the flushRoot option to get it to take
-- effect.
myExtraPackages =
    ["debian-archive-keyring"] ++
    case releaseRepoName myBaseRelease of
      "debian" -> []
      "ubuntu" -> ["ubuntu-keyring"]
      _ -> error $ "Unknown base release: " ++ myBaseRelease

-- Specify extra packages to include as essential in the build
-- environment.  This option was provided to add either upstart or
-- sysvinit to the build when they ceased to be 'Required' packages.
myExtraEssential =
    ["belocs-locales-bin", "gnupg", "dpkg"] ++
    case releaseRepoName myBaseRelease of
      "debian" -> []
      "ubuntu" -> ["upstart-compat-sysv"]
      _ -> error $ "Unknown base release: " ++ myBaseRelease

------------------------- SOURCES --------------------------------

debianSourceLines release =
    [ "deb http://" ++ myDebianMirrorHost ++ "/debian " ++ release ++ " main contrib non-free"
    , "deb-src http://" ++ myDebianMirrorHost ++ "/debian " ++ release ++ " main contrib non-free" ]

ubuntuSourceLines release =
    [ "deb http://" ++ myUbuntuMirrorHost ++ "/ubuntu/ " ++ release ++ " main restricted universe multiverse"
    , "deb-src http://" ++ myUbuntuMirrorHost ++ "/ubuntu/ " ++ release ++ " main restricted universe multiverse"
    , "deb http://" ++ myUbuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-updates main restricted universe multiverse"
    , "deb-src http://" ++ myUbuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-updates main restricted universe multiverse"
    , "deb http://" ++ myUbuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-backports main restricted universe multiverse"
    , "deb-src http://" ++ myUbuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-backports main restricted universe multiverse"
    , "deb http://" ++ myUbuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-security main restricted universe multiverse"
    , "deb-src http://" ++ myUbuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-security main restricted universe multiverse" ]

baseReleaseSourceLines release =
    case releaseRepoName release of
      "debian" -> debianSourceLines release
      "ubuntu" -> ubuntuSourceLines release

seereasonSourceLines release =
    releaseSourceLines (dropSuffix "-seereason" release) ++
    [ "deb http://deb.seereason.com/" ++ releaseRepoName release ++ " " ++ release ++ " main"
    , "deb-src http://deb.seereason.com/" ++ releaseRepoName release ++ " " ++ release ++ " main" ]

privateSourceLines release =
    releaseSourceLines (dropSuffix "-private" release) ++
    [ "deb " ++ myPrivateUploadURI ++ "/" ++ releaseRepoName release ++ " " ++ release ++ " main"
    , "deb-src " ++ myPrivateUploadURI ++ "/" ++ releaseRepoName release ++ " " ++ release ++ " main" ]

releaseSourceLines :: String -> [String]
releaseSourceLines release =
    case () of
      _ | isSuffixOf "-private" release -> privateSourceLines release
        | isSuffixOf "-seereason" release -> seereasonSourceLines release
        | True -> baseReleaseSourceLines release

releaseSources release = (release, unlines (releaseSourceLines release))

allPrivateSources = map releaseSources (map (++ "-seereason-private") (debianReleases ++ ubuntuReleases))

topReleaseName :: String -> String
topReleaseName name =
    foldr dropSuff name ["-seereason", "-private"]
    where dropSuff suff name = if isSuffixOf suff name then dropSuffix suff name else name

debianReleases = ["sid", "lenny"]
ubuntuReleases = ["jaunty", "intrepid", "hardy"]

dropSuffix suff x = take (length x - length suff) x

allSources =
    map releaseSources (debianReleases ++ ubuntuReleases ++ map (++ "-seereason") (debianReleases ++ ubuntuReleases)) ++
    [("debian-experimental", unlines (debianSourceLines "experimental")),
     ("debian-multimedia",
      (unlines ["deb http://mirror.home-dn.net/debian-multimedia stable main",
                "deb-src http://mirror.home-dn.net/debian-multimedia stable main"])),
      ("kanotix",
       (unlines ["deb http://kanotix.com/files/debian sid main contrib non-free vdr",
                 "  deb-src http://kanotix.com/files/debian sid main contrib non-free vdr"]))] ++
    if myBuildPrivateTargets then allPrivateSources else []

----------------------- BUILD RELEASE ----------------------------

releaseRepoName name
    | elem name (debianReleases ++ oldDebianReleases) = "debian"
    | elem name (ubuntuReleases ++ oldUbuntuReleases) = "ubuntu"
    | isSuffixOf "-seereason" name = releaseRepoName (dropSuffix "-seereason" name)
    | isSuffixOf "-private" name = releaseRepoName (dropSuffix "-private" name)
    | True = error $ "Unknown release name: " ++ show name

oldDebianReleases = ["etch", "sarge"]
oldUbuntuReleases = ["gutsy", "feisty"]

------------------------ TARGETS ---------------------

ghc610CoreTargets =
    [ "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bzlib/0.5.0.0/bzlib-0.5.0.0.tar.gz:ab594aaf9998ed602f8b23dd25199e19):(darcs:http://src.seereason.com/ghc610/debian/haskell-bzlib-debian)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/zlib/0.5.0.0/zlib-0.5.0.0.tar.gz:22fa6d394c42c8584b234799b923f860):(darcs:http://src.seereason.com/ghc610/debian/haskell-zlib-debian)"
    , "darcs:http://src.seereason.com/ghc610/haskell-cdbs"
    , "darcs:http://src.seereason.com/ghc610/haskell-unixutils"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cpphs/1.6/cpphs-1.6.tar.gz:8a7565ff3b2d7bdb594af4c10c594951):(darcs:http://src.seereason.com/ghc610/debian/cpphs-debian)"
    , "quilt:(apt:sid:haxml):(darcs:http://src.seereason.com/ghc610/quilt/haxml-quilt)"
    , "darcs:http://src.seereason.com/ghc610/haskell-extra"
    , "darcs:http://src.seereason.com/ghc610/haskell-debian-3"
    , "deb-dir:(uri:http://www.haskell.org/ghc/dist/6.10.1/ghc-6.10.1-src.tar.bz2:54c676a632b3d73cf526b06347522c32):(darcs:http://src.seereason.com/ghc610/debian/ghc610-debian)"
    , "quilt:(uri:http://ftp.de.debian.org/debian/pool/main/h/haskell-devscripts/haskell-devscripts_0.6.15.tar.gz:996acac2c6fb2da2be9c5016f93a3c67):(darcs:http://src.seereason.com/ghc610/quilt/haskell-devscripts-quilt)"
    ]

autobuilderTargets =
    [ "darcs:http://src.seereason.com/ghc610/build-env"
    , "darcs:http://src.seereason.com/ghc610/autobuilder"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cgi/3001.1.7.1/cgi-3001.1.7.1.tar.gz:02b1d2fe6f271a17c1eb8b897fbd1d7f):(darcs:http://src.seereason.com/ghc610/debian/haskell-cgi-debian)"
    , "darcs:http://src.seereason.com/ghc610/haskell-mime"
    , "quilt:(apt:sid:magic-haskell):(darcs:http://src.seereason.com/ghc610/quilt/magic-haskell-quilt)"
    ]

ghc610Targets =
    [ "quilt:(apt:sid:haskell-utils):(darcs:http://src.seereason.com/ghc610/quilt/haskell-utils-quilt)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/applicative-extras/0.1.3/applicative-extras-0.1.3.tar.gz:50fa4c61e89654ea9858c304b4682680):(darcs:http://src.seereason.com/ghc610/debian/applicative-extras-debian)"
    , "darcs:http://src.seereason.com/ghc610/formlets"
    , "quilt:(apt:hardy:haskell-binary):(darcs:http://src.seereason.com/ghc610/quilt/haskell-binary-quilt)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/extensible-exceptions/0.1.1.0/extensible-exceptions-0.1.1.0.tar.gz:7aba82acc64fa2f2dc89d8ac27e24a43):(darcs:http://src.seereason.com/ghc610/debian/extensible-exceptions-debian)"
    , "cd:happstack-util:darcs:http://src.seereason.com/happstack"
    , "cd:happstack-data:darcs:http://src.seereason.com/happstack"
    , "cd:happstack-ixset:darcs:http://src.seereason.com/happstack"
    , "cd:happstack-server:darcs:http://src.seereason.com/happstack"
    , "cd:happstack-state:darcs:http://src.seereason.com/happstack"
    , "cd:happstack-util:darcs:http://src.seereason.com/happstack"
    , "darcs:http://src.seereason.com/happstack-extra"
    , "darcs:http://src.seereason.com/haskell-help"
    , "deb-dir:(darcs:http://haskell.org/~kolmodin/code/hinotify):(darcs:http://src.seereason.com/ghc610/debian/hinotify-debian)"
    , "quilt:(apt:sid:haskell-hspread):(darcs:http://src.seereason.com/ghc610/quilt/haskell-hspread-quilt)"
    , "quilt:(apt:sid:haskell-utf8-string):(darcs:http://src.seereason.com/ghc610/quilt/haskell-utf8-string-quilt)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happy/1.18.2/happy-1.18.2.tar.gz:adb1679a1fa8cec74a6e621a4a277e98):(darcs:http://src.seereason.com/ghc610/debian/happy-debian)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/haskell-src-exts/0.4.3.1/haskell-src-exts-0.4.3.1.tar.gz:4ff97fdae2bca0da0194fcb80974b188):(darcs:http://src.seereason.com/ghc610/debian/haskell-src-exts-debian)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/RJson/0.3.5/RJson-0.3.5.tar.gz:e69c34b295e067c169a15fc5327a9dd9):(darcs:http://src.seereason.com/ghc610/debian/RJson-debian)"
    , "darcs:http://src.seereason.com/ghc610/iconv"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hslogger/1.0.7/hslogger-1.0.7.tar.gz:74ff79b2abfec7e24b96925f06112c9f):(darcs:http://src.seereason.com/ghc610/debian/hslogger-debian)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HTTP/4000.0.4/HTTP-4000.0.4.tar.gz:6526c1ee59cd3aedc7aa380673c80ef1):(darcs:http://src.seereason.com/ghc610/debian/haskell-http-debian)"
    , "darcs:http://src.seereason.com/ghc610/syb-with-class"
    , "darcs:http://src.seereason.com/ghc610/HAppS-Util"
    , "darcs:http://src.seereason.com/ghc610/HAppS-Data"
    , "darcs:http://src.seereason.com/ghc610/HAppS-IxSet"
    , "darcs:http://src.seereason.com/ghc610/HAppS-State"
    , "darcs:http://src.seereason.com/ghc610/HAppS-Server"
    , "deb-dir:(darcs:http://code.haskell.org/HSP/harp):(darcs:http://src.seereason.com/ghc610/debian/harp-debian)"
    , "deb-dir:(darcs:http://code.haskell.org/HSP/hjavascript):(darcs:http://src.seereason.com/ghc610/debian/hjavascript-debian)"
    , "darcs:http://src.seereason.com/ghc610/hsx"
    , "deb-dir:(darcs:http://src.seereason.com/ghc610/hsp):(darcs:http://src.seereason.com/ghc610/debian/hsp-debian)"
    , "darcs:http://src.seereason.com/ghc610/happs-hsp-formlets"
    , "deb-dir:(darcs:http://code.haskell.org/HSP/hsx-xhtml):(darcs:http://src.seereason.com/ghc610/debian/hsx-xhtml-debian)"
    , "deb-dir:(darcs:http://code.haskell.org/HSP/hjscript):(darcs:http://src.seereason.com/ghc610/debian/hjscript-debian)"
    , "darcs:http://src.seereason.com/ghc610/HAppS-Extra"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Shellac/0.9.1/Shellac-0.9.1.tar.gz:0a563883b3acedb9c0d4308b44772f0f):(darcs:http://src.seereason.com/ghc610/debian/shellac-debian)"
    , "darcs:http://src.seereason.com/ghc610/frisby"
    , "darcs:http://src.seereason.com/ghc610/decimal"
    , "darcs:http://src.seereason.com/vc-darcs"
    , "deb-dir:(darcs:http://darcs.haskell.org/cabal-install):(darcs:http://src.seereason.com/ghc610/debian/cabal-install-debian)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/uniplate/1.2.0.3/uniplate-1.2.0.3.tar.gz:e0e10700870f5b9756d4097e640164ca):(darcs:http://src.seereason.com/ghc610/debian/uniplate-debian)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/i18n/0.3/i18n-0.3.tar.gz:e59445b4ad743ab77c61a281cf942bbf):(darcs:http://src.seereason.com/ghc610/debian/i18n-debian)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/stb-image/0.1.1/stb-image-0.1.1.tar.gz:9e8ac1305c60e13d04359744976e402a):(darcs:http://src.seereason.com/ghc610/debian/stb-image-debian)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/gd/3000.4.0/gd-3000.4.0.tar.gz:7bc5bb68638b807d592aba433beb3fa5):(darcs:http://src.seereason.com/ghc610/debian/haskell-gd-debian)"
    , "deb-dir:(uri:http://hackage.haskell.org/packages/archive/CC-delcont/0.2/CC-delcont-0.2.tar.gz:e52149fca9bf76330a7c159917152790):(darcs:http://src.seereason.com/ghc610/debian/CC-delcont-debian)"
    ]

otherTargets = ["darcs:http://src.seereason.com/tree-widget"]

privateTargets =
    ["darcs:" ++ myPrivateDarcsURI ++ "/haskell-filecache",
     "darcs:" ++ myPrivateDarcsURI ++ "/haskell-document",
     "darcs:" ++ myPrivateDarcsURI ++ "/haskell-appraisal",
     "darcs:" ++ myPrivateDarcsURI ++ "/mailingList",
     "darcs:" ++ myPrivateDarcsURI ++ "/generic-formlets",
     "darcs:" ++ myPrivateDarcsURI ++ "/AlgebraZam",
     "darcs:" ++ myPrivateDarcsURI ++ "/SeniorityMatters"]

---------------------------- THE PARAMETERS RECORD ---------------------------------

params =
    ParamRec
    { verbosity = myVerbosity,
      -- The directory the program will use for its working storage.
      -- Normally this is not specified, in which case ~/.autobuilder
      -- is used.
      topDirParam = Nothing,
      -- Unspecified debugging behavior.
      debug = False,
      dryRun = False,
      -- This flag says not to do anything that will affect the
      -- outside world, such as uploads and remote newdists.  However,
      -- the files in ~/.autobuilder may still be modified when this
      -- is used.  It does avoids making extensive changes to the
      -- local repository by exiting as soon as a target it identified
      -- as needing to be built.
      requiredVersion = [(parseDebianVersion "4.39",Nothing)],
      -- Print the sources.list for the build distro and exit.
      showSources = False,
      -- Print the expanded runtime parameter list and continue.
      showParams = False,
      -- Remove and re-create the entire autobuilder working directory (topDir.)
      flushAll = False,
      -- Load the most recent cached repository information from
      -- ~/.autobuilder/repoCache and assume that it is still good -
      -- that no releases have been added or removed from the
      -- repositories listed.  This is usually safe and saves some
      -- time querying each remote repository before using it.
      useRepoCache = True,
      -- Specify all known source.list files, associating a name
      -- with each one.  The names can be used in apt targets.
      sources = allSources,
      -- Specify one or more build targets, methods for obtaining the
      -- source code of a package to be built.  See TARGET TYPES below
      -- for information about the available target types."
      targets = myTargets,
      -- Specify a source package which we want to build, and stop
      -- once all goals are built.  If not given all targets are
      -- considered goals.  (As of version 4.41 this option is not be
      -- fully functional, sometimes specifying goals will prevent all
      -- the builds.)
      goals = myGoals,
      -- Obsolete
      omitTargets = [],
      -- The string used to construct modified version numbers.
      vendorTag = myVendorTag,
      -- use the old \"r0vendor1\" style tag instead of just \"vendor1\"
      extraReleaseTag = Nothing,
      -- Discard and re-download all source code before building.
      flushSource = False,
      -- Build the named source package(s) whether or not they seems
      -- to need it.
      forceBuild = myForceBuild,
      -- Normally, if a build dependency has an older version number
      -- than it did on a previous build, it is an error.  This
      -- generally means the sources.list is incorrect.  However, this
      -- flag can be necessary if a package gets withdrawn from the build
      -- or base release.
      allowBuildDependencyRegressions = False,
      -- When selecting build dependencies, prefer this particular
      -- package over other alternatives that could fulfill the
      -- dependency, even if this package seems older than some other
      -- alternative.  For example, the c-compiler virtual package is
      -- provided by gcc-3.3, gcc-3.4, gcc-4.0, etc.  If 'Prefer:
      -- gcc-3.4' is used, a dependency on c-compiler will choose
      -- gcc-3.4 over the others if possible.
      preferred = [],
      -- Specify how strict to be about the creation of build
      -- environments, trading off correctness with speed.  In all
      -- cases, a clean build environment is always maintained, and
      -- copied before the package build is performed using rsync.
      -- 'Strict' means the clean build environment is discarded and
      -- recreated before each target is built.  'Moderate' means the
      -- clean build environment is kept between successive runs, and
      -- updated as necessary using 'apt-get update' and 'apt-get
      -- dist-upgrade'.  'Lax' means that build dependencies are
      -- installed into the clean build environment so that they
      -- accumulate across runs.
      strictness = P.Moderate,
      -- Set one or more environment variables during the build, e.g. 
      -- setEnv = ["DEBIAN_KERNEL_JOBS=5"].
      setEnv = [],
      -- Obsolete?  Add a missing build dependency.
      buildDepends = [],
      -- Prevent the appearance of a new binary package from
      -- triggering builds of its build dependencies.  Optionally, a
      -- particular source package can be specified whose rebuild will
      -- be prevented.  This is used to break dependency loops, For
      -- example, 'Relax-Depends: ghc6 hscolour' means 'even if ghc6
      -- is rebuilt, don't rebuild hscolour even though ghc6 is one of
      -- its build dependencies.
      relaxDepends = RelaxInfo [(BinPkgName "cabal-debian",Just (SrcPkgName "haskell-cpphs")),
                                (BinPkgName "happy",Just (SrcPkgName "haskell-happy")),
                                (BinPkgName "haddock",Just (SrcPkgName "haskell-haddock")),
                                (BinPkgName "ghc6",Just (SrcPkgName "ghc6")),
                                (BinPkgName "base-files",Nothing),
                                (BinPkgName "bash",Nothing),
                                (BinPkgName "bsdutils",Nothing),
                                (BinPkgName "devscripts",Nothing),
                                (BinPkgName "dpkg",Nothing),
                                (BinPkgName "dpkg-dev",Nothing),
                                (BinPkgName "gcc",Nothing),
                                (BinPkgName "g++",Nothing),
                                (BinPkgName "make",Nothing),
                                (BinPkgName "mount",Nothing),
                                (BinPkgName "base-passwd",Nothing),
                                (BinPkgName "mktemp",Nothing),
                                (BinPkgName "sed",Nothing),
                                (BinPkgName "util-linux",Nothing),
                                (BinPkgName "sysvinit-utils",Nothing),
                                (BinPkgName "libghc6-ghc-paths-prof",Just (SrcPkgName "haskell-haddock")),
                                (BinPkgName "ghc6",Just (SrcPkgName "haskell-haddock")),
                                (BinPkgName "ghc6-prof",Just (SrcPkgName "haskell-haddock")),
                                (BinPkgName "ghc6-doc",Just (SrcPkgName "haskell-haddock")),
                                (BinPkgName "haskell-devscripts-cdbs",Just (SrcPkgName "haskell-haddock")),
                                (BinPkgName "ghc6",Just (SrcPkgName "hscolour")),
                                (BinPkgName "xsltproc",Just (SrcPkgName "ghc6")),
                                (BinPkgName "cabal-debian",Just (SrcPkgName "haskell-debian")),
                                (BinPkgName "cabal-debian",Just (SrcPkgName "haskell-extra")),
                                (BinPkgName "cabal-debian",Just (SrcPkgName "haskell-cabal")),
                                (BinPkgName "cabal-debian",Just (SrcPkgName "haskell-ghc-paths")),
                                (BinPkgName "libghc6-debian-dev",Just (SrcPkgName "haskell-cabal-debian")),
                                (BinPkgName "libghc6-debian-prof",Just (SrcPkgName "haskell-cabal-debian")),
                                (BinPkgName "libghc6-debian-doc",Just (SrcPkgName "haskell-cabal-debian")),
                                (BinPkgName "haskell-devscripts",Just (SrcPkgName "ghc6")),
                                (BinPkgName "haddock",Just (SrcPkgName "ghc6")),
                                (BinPkgName "autoconf",Nothing),
                                (BinPkgName "debhelper",Nothing),
                                (BinPkgName "debianutils",Nothing),
                                (BinPkgName "diff",Nothing),
                                (BinPkgName "e2fsprogs",Nothing),
                                (BinPkgName "findutils",Nothing),
                                (BinPkgName "flex",Nothing),
                                (BinPkgName "login",Nothing),
                                (BinPkgName "coreutils",Nothing),
                                (BinPkgName "grep",Nothing),
                                (BinPkgName "gs",Nothing),
                                (BinPkgName "gzip",Nothing),
                                (BinPkgName "hostname",Nothing),
                                (BinPkgName "intltool",Nothing),
                                (BinPkgName "ncurses-base",Nothing),
                                (BinPkgName "ncurses-bin",Nothing),
                                (BinPkgName "perl",Nothing),
                                (BinPkgName "perl-base",Nothing),
                                (BinPkgName "tar",Nothing),
                                (BinPkgName "sysvinit",Nothing),
                                (BinPkgName "libc6-dev",Nothing),
                                (BinPkgName "module-init-tools",Just (SrcPkgName "linux-2.6"))],
      -- Run dpkg-buildpackage with the -nc argument.  This also
      -- disables syncing with the clean source tree.  This should
      -- only be used for debugging the autobuilder or for debugging
      -- the package build.  To edit the package you need to find the
      -- work directory in the cached build and make your edits there.
      -- Then you will need to check them back into your revision
      -- control system.
      noClean = False,
      extraPackages = myExtraPackages,
      extraEssential = myExtraEssential,
      -- Specify packages for build-env to remove from the essential
      -- list even if they are marked essential
      omitEssential = [],
      -- OBSOLETE: Don't automatically consider all the build
      -- essential packages to be build dependencies.  If you are
      -- working with an unstable repository where the core packages
      -- are undergoing frequent revisions, and you aren't worried
      -- that a new version of 'tar' is going to change the outcome of
      -- your builds, this option can reduce the number of pointless
      -- rebuilds.  (But try relaxDepends first.)
      omitBuildEssential = False,
      -- Packages uploaded to the build release will be compatible
      -- with packages in this release.
      baseRelease = SliceName {sliceName = myBaseRelease},
      -- The name of the release we will be uploading to.
      buildRelease = ReleaseName {relName = myBaseRelease ++ (if myBuildPrivateTargets then "-private" else "-seereason")},
      -- Don't modify the package's version in any way before
      -- building.  Normally a tag is added to signify the vendor and
      -- the base release of the package.  Using this option can lead
      -- to attempts to upload packages that are already present in
      -- the repository, or packages that are trumped by versions
      -- already uploaded to the release.
      doNotChangeVersion = False,
      -- Signifies that the release we are building for is a development
      -- (or unstable) release.  This means we the tag we add doesn't need
      -- to include '~release', since there are no newer releases to
      -- worry about trumping.
      isDevelopmentRelease = elem (topReleaseName myBaseRelease) ["sid", "jaunty"],
      -- Use these aliases for the release name when constructing the
      -- vendor tag used in the version number extension of built
      -- packages.
      releaseAliases = [("etch", "bpo40+"),
                        ("hardy-seereason", "hardy"),
                        ("intrepid-seereason", "intrepid"),
                        ("jaunty-seereason", "jaunty")],
      -- Discard and recreate the clean build environment.
      flushRoot = False,
      -- Do a garbage collection on the local repository, move all
      -- unreferenced files to 'removed'.  This is probably not a
      -- useful option, as the local repository is frequently removed.
      cleanUp = False,
      -- The list of architectures to prepare the repository to accept.
      archList = [Binary "i386",Binary "amd64"],
      -- Discard the packages in the local pool before building.  Use
      -- this when a bad package was uploaded to the local pool that
      -- you don't want uploaded to the remote pool.
      flushPool = False,
      -- After a successful build of all the targets, dupload all the
      -- packages in the local pool specified by the uploadURI
      -- argument to the corresponding repository.
      doUpload = myDoUpload,
      -- After uploading, run newdist on the remote repository
      -- incoming directory
      doNewDist = myDoNewDist,
      -- Use given executable as the newdist program, the program that
      -- runs on the upload host to install packages in the incoming
      -- directory to the repository. | 
      newDistProgram = "newdist -v",
      uploadHost = Just myUploadHost,
      buildURI = myBuildURI,
      uploadURI = myUploadURI,
      -- Pass a --create NAME argument to newdist to create a new
      -- release in the upload repository.
      createRelease = [],
      -- What to do if the sources.list changes in the
      -- configuration directory.  The argument may be
      --   SourcesChangedError - (the default) print a message and exit, 
      --   SourcesChangedUpdate - rewrite sources.list and update the environment, 
      --   SourcesChangedRemove - discard and rebuild the environment
      ifSourcesChanged = SourcesChangedError,
      -- Try to set up ssh keys if upload host asks for a password.
      doSSHExport = False,
      -- Email address of autobuilder for use in generated changelog entries.
      autobuilderEmail = "SeeReason Autobuilder <autobuilder@seereason.org>"
    }
