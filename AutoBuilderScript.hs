#!/usr/bin/env runhaskell -package=base-3.0.3.0
import Data.Maybe
import qualified Debian.AutoBuilder.Main as M
import qualified Debian.AutoBuilder.ParamClass as P
import Debian.AutoBuilder.ParamRec
import Debian.GenBuildDeps (SrcPkgName(SrcPkgName), BinPkgName(BinPkgName), RelaxInfo(RelaxInfo))
import Debian.Repo.Cache (SourcesChangedAction(SourcesChangedError))
import Debian.Repo.Types (SliceName(SliceName, sliceName), ReleaseName(ReleaseName, relName), Arch(Binary))
import Debian.URI
import Debian.Version
import System.IO (hPutStrLn, stderr)

main =
    hPutStrLn stderr "Autobuilder starting..." >>
    M.main [params]

params = addTargets (setBaseRelease "hardy" defaultParams)

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
    , "cd:HAppS-Util:darcs:http://src.seereason.com/happstack"
    , "cd:HAppS-Data:darcs:http://src.seereason.com/happstack"
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

addTargets params = params {targets = ghc610CoreTargets ++ autobuilderTargets ++ ghc610Targets ++ otherTargets}

------------------------- SOURCES --------------------------------

debianSources dist =
    [ " deb http://" ++ debianMirrorHost ++ "/debian " ++ dist ++ " main contrib non-free"
    , " deb-src http://" ++ debianMirrorHost ++ "/debian " ++ dist ++ " main contrib non-free" ]

ubuntuSources dist =
    [ "  deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ dist ++ " main restricted universe multiverse",
      "  deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ dist ++ " main restricted universe multiverse",
      "  deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ dist ++ "-updates main restricted universe multiverse",
      "  deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ dist ++ "-updates main restricted universe multiverse",
      "  deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ dist ++ "-backports main restricted universe multiverse",
      "  deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ dist ++ "-backports main restricted universe multiverse",
      "  deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ dist ++ "-security main restricted universe multiverse",
      "  deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ dist ++ "-security main restricted universe multiverse"]

seereasonSources baseDist =
    [ " deb http://deb.seereason.com/" ++ repoFromRelease baseDist ++ " " ++ baseDist ++ "-seereason main"
    , " deb-src http://deb.seereason.com/" ++ repoFromRelease baseDist ++ " " ++ baseDist ++ "-seereason main" ]

sid = unlines ("sid" : debianSources "sid")
etch = unlines ("etch" : debianSources "etch")
lenny = unlines ("lenny" : debianSources "lenny")
sarge = unlines ("sarge" : debianSources "sarge")
sidSeereason = unlines ("sid-seereason" : debianSources "sid" ++ seereasonSources "sid")
lennySeereason = unlines ("lenny-seereason" : debianSources "lenny" ++ seereasonSources "lenny")
feisty = unlines ("feisty" : ubuntuSources "feisty")
gutsy = unlines ("gutsy" : ubuntuSources "gutsy")
hardy = unlines ("hardy" : ubuntuSources "hardy")
intrepid = unlines ("intrepid" : ubuntuSources "intrepid")
jaunty = unlines ("jaunty" : ubuntuSources "jaunty")
hardySeereason = unlines ("hardy-seereason" : ubuntuSources "hardy" ++ seereasonSources "hardy")
intrepidSeereason = unlines ("intrepid-seereason" : ubuntuSources "intrepid" ++ seereasonSources "intrepid")
jauntySeereason = unlines ("jaunty-seereason" : ubuntuSources "jaunty" ++ seereasonSources "jaunty")
debianExperimental = unlines ("debian-experimental" : debianSources "experimental")
debianMultimedia =
    unlines ["debian-multimedia",
             "  deb http://mirror.home-dn.net/debian-multimedia stable main",
             "  deb-src http://mirror.home-dn.net/debian-multimedia stable main"]
kanotix =
    unlines ["kanotix",
             "  deb http://kanotix.com/files/debian sid main contrib non-free vdr",
             "  deb-src http://kanotix.com/files/debian sid main contrib non-free vdr"]

----------------------- BUILD RELEASE ----------------------------

setBaseRelease x params =
    params {baseRelease = SliceName {sliceName = x},
            buildRelease = ReleaseName {relName = x ++ "-seereason"}}

repoFromRelease "feisty" = "ubuntu"
repoFromRelease "gutsy" = "ubuntu"
repoFromRelease "hardy" = "ubuntu"
repoFromRelease "intrepid" = "ubuntu"
repoFromRelease "jaunty" = "ubuntu"
repoFromRelease "etch" = "debian"
repoFromRelease "sarge" = "debian"
repoFromRelease "lenny" = "debian"
repoFromRelease "sid" = "debian"
repoFromRelease x = error $ "Invalid base release name: " ++ show x

----------------------- DEFAULTS ---------------------------------

defaultParams =
    ParamRec
    { verbosity = 2,
      topDirParam = Nothing,
      debug = False,
      dryRun = False,
      requiredVersion = [(parseDebianVersion "2.26",Nothing)],
      showSources = False,
      showParams = False,
      flushAll = False,
      useRepoCache = True,
      sources = [sid, etch, lenny, sarge, 
                 sidSeereason, lennySeereason, 
                 feisty, gutsy, hardy, intrepid, jaunty, 
                 hardySeereason, intrepidSeereason, jauntySeereason, 
                 debianExperimental, debianMultimedia, kanotix],
      targets = [],
      goals = [],
      omitTargets = [],
      vendorTag = "seereason",
      extraReleaseTag = Nothing,
      flushSource = False,
      forceBuild = False,
      allowBuildDependencyRegressions = False,
      preferred = [],
      strictness = P.Moderate,
      setEnv = [],
      buildDepends = [],
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
      noClean = False,
      extraPackages = ["ubuntu-keyring", "debian-archive-keyring", "seereason-keyring"],
      extraEssential = ["belocs-locales-bin", "upstart-compat-sysv", "gnupg", "dpkg"],
      omitEssential = [],
      omitBuildEssential = False,
      baseRelease = SliceName {sliceName = ""},
      buildRelease = ReleaseName {relName = ""},
      doNotChangeVersion = False,
      isDevelopmentRelease = False,
      releaseAliases = [("etch", "bpo40+"),("hardy-seereason", "hardy"),("intrepid-seereason", "intrepid"),("jaunty-seereason", "jaunty")],
      flushRoot = False,
      cleanUp = False,
      archList = [Binary "i386",Binary "amd64"],
      flushPool = False,
      doUpload = True,
      doNewDist = True,
      newDistProgram = "newdist -v",
      uploadHost = Just "deb.seereason.com",
      buildURI = parseURI "http://deb.seereason.com/ubuntu",
      uploadURI = parseURI "ssh://upload@deb.seereason.com/srv/deb/ubuntu",
      createRelease = [],
      ifSourcesChanged = SourcesChangedError,
      doSSHExport = False,
      autobuilderEmail = "SeeReason Autobuilder <autobuilder@seereason.org>"
    }

--------------------------- LOCAL ------------------------------

ubuntuMirrorHost = "mirror.anl.gov"
debianMirrorHost = "mirror.anl.gov"
