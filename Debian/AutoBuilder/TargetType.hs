{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.TargetType
    ( Target(Target, tgt, cleanSource)
    , prepareTarget
    , targetName
    , targetRelaxed
    , targetControl
    , getRelaxedDependencyInfo
    ) where

import Control.Applicative.Error (Failing(Success, Failure), failing)
import Control.Exception (catch, throw)
import Control.Monad(when)
import Control.Monad.Trans (liftIO)
import Debian.AutoBuilder.Types.Buildable (Buildable(..))
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import Debian.Changes (logVersion, ChangeLogEntry(..))
import Debian.Control (Control, Control'(Control), fieldValue,  Paragraph'(Paragraph), Field'(Comment), parseControlFromFile)
import qualified Debian.GenBuildDeps as G
import Debian.Relation.ByteString(Relations)
import Debian.Repo.OSImage (OSImage)
import Debian.Repo.SourceTree (DebianBuildTree(..), control, entry, subdir, debdir, {-findOneDebianBuildTree,-} findDebianBuildTree, findDebianBuildTrees, copyDebianBuildTree,
                               DebianSourceTree(..), findDebianSourceTree, copyDebianSourceTree)
import Debian.Repo.Types (AptCache(rootDir), EnvRoot(rootPath))
import qualified Debian.Version
import Prelude hiding (catch)
import System.Directory(renameDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Files (createLink, removeLink)
import System.Unix.QIO (qPutStrLn, quieter)

-- | Information collected from the build tree for a Tgt.
data Target
    = Target { tgt :: Buildable			-- ^ The instance of BuildTarget
             , cleanSource :: DebianBuildTree	-- ^ The source code stripped of SCCS info
             , targetDepends :: G.DepInfo	-- ^ The dependency info parsed from the control file
             }

instance Eq Target where
    a == b = targetName a == targetName b

-- |Prepare a target for building in the given environment.  At this
-- point, the target needs to be a DebianSourceTree or a
-- DebianBuildTree. 
prepareTarget :: P.CacheRec -> Relations -> OSImage -> Buildable -> IO Target
prepareTarget cache globalBuildDeps os source =
    quieter (+ 2) $ prepareBuild cache os (download source) >>= \ tree ->
    getTargetDependencyInfo globalBuildDeps tree >>=
    failing (error . show)
            (\ deps -> return $ Target { tgt = source
                                       , cleanSource = tree
                                       , targetDepends = deps })

targetControl :: Target -> Control
targetControl = control . cleanSource

-- |'prepareBuild' returns a Debian build tree for a target with all
-- the revision control files associated with the old target removed.
-- This ensures that the tarball and\/or the .diff.gz file in the deb
-- don't contain extra junk.  It also makes sure that debian\/rules is
-- executable.
prepareBuild :: P.CacheRec -> OSImage -> T.Download -> IO DebianBuildTree
prepareBuild _cache os target =
    do (_, trees) <- findDebianBuildTrees top
       case filter checkName trees of
         [] ->
             qPutStrLn ("Build tree not found in " ++ top ++ ", creating new one") >>
             findDebianSourceTree top >>=
             copySource
         [tree] ->
             copyBuild tree
         _ ->
             error ("Multiple build trees found")
{-         
       debBuild <- findOneDebianBuildTree top
       case debBuild of
         Success tree -> copyBuild tree
         Failure msgs ->
             qPutStrLn ("Build tree not found in " ++ top ++ ", creating new one\n  " ++ intercalate "\n  " msgs) >>
             findDebianSourceTree top >>=
             copySource
-}
    where
      checkName _ = True
{-
      checkName tree = source == Just name
          where source = fieldValue "Source" (head (unControl (control' (debTree' tree))))
-}
      top = T.getTop target
      copySource :: DebianSourceTree -> IO DebianBuildTree
      copySource debSource =
          do let name = logPackage . entry $ debSource
                 dest = rootPath (rootDir os) ++ "/work/build/" ++ name
                 ver = Debian.Version.version . logVersion . entry $ debSource
                 newdir = escapeForBuild $ name ++ "-" ++ ver
             --io $ System.IO.hPutStrLn System.IO.stderr $ "copySource " ++ show debSource
             _copy <- copyDebianSourceTree debSource (dest </> newdir)
             -- Clean the revision control files for this target out of the copy of the source tree
             (_out, _time) <- T.cleanTarget target (dest </> newdir)
             maybe (return ()) (liftIO . copyOrigTarball dest name ver) (T.origTarball target)
             findDebianBuildTree dest newdir
      copyBuild :: DebianBuildTree -> IO DebianBuildTree
      copyBuild debBuild =
          do let name = logPackage . entry $ debBuild
                 dest = rootPath (rootDir os) ++ "/work/build/" ++ name
                 ver = Debian.Version.version . logVersion . entry $ debBuild
                 newdir = escapeForBuild $ name ++ "-" ++ ver
             --io $ System.IO.hPutStrLn System.IO.stderr $ "copyBuild " ++ show debBuild
             _copy <- copyDebianBuildTree debBuild dest
             (_output, _time) <- T.cleanTarget target (dest </> newdir)
             when (newdir /= (subdir debBuild))
                      (liftIO $ renameDirectory (dest ++ "/" ++ subdir debBuild) (dest ++ "/" ++ newdir))
             findDebianBuildTree dest newdir

      copyOrigTarball dest name ver src =
          hPutStrLn stderr ("forceLink " ++ src ++ " " ++ dest ++ "/" ++ name ++ "-" ++ ver ++ ".orig.tar" ++ takeExtension src) >>
          forceLink src (dest ++ "/" ++ name ++ "_" ++ ver ++ ".orig.tar" ++ takeExtension src)

-- |calls 'createSymbolicLink' but will remove the target and retry if
-- 'createSymbolicLink' raises EEXIST.
forceLink :: FilePath -> FilePath -> IO ()
forceLink target linkName =
    createLink target linkName `catch`
      (\ e -> if isAlreadyExistsError e 
              then do removeLink linkName
                      createLink target linkName
              else throw e)

-- |Make a path "safe" for building.  This shouldn't be necessary,
-- but various packages make various assumptions about the type
-- of characters that appear in the name of the working directory
-- that the build is performed in.  For example, kdenetwork objects
-- to the colon, kdebase objects to the plus sign, and so on.
escapeForBuild :: FilePath -> FilePath
escapeForBuild =
    map escape
    where
      escape ':' = '_'
      escape '+' = '_'
      escape c = c

-- | The /Source:/ attribute of debian\/control.
targetName :: Target -> String
targetName target =
    case removeCommentParagraphs (targetControl target) of
      (Control (paragraph : _)) ->
          maybe (error "Missing Source field") id $ fieldValue "Source" paragraph
      _ -> error "Target control information missing"

removeCommentParagraphs (Control paragraphs) =
    Control (filter (not . isCommentParagraph) paragraphs)
    where
      isCommentParagraph (Paragraph fields) = all isCommentField fields
      isCommentField (Comment _) = True
      isCommentField _ = False

{-
updateDependencyInfo :: G.RelaxInfo -> Relations -> [Target] -> IO [Target]
updateDependencyInfo relaxInfo globalBuildDeps targets =
    q12 "Updating dependency info" $
    getDependencyInfo globalBuildDeps targets >>=
    (\ ts -> qPutStrLn ("Original dependencies: " ++ show (map (prettyTarget . targetDepends) ts)) >> return ts) >>=
    (\ ts -> qPutStrLn ("Relaxed dependencies:  " ++ show (map (prettyTarget . targetRelaxed relaxInfo) ts)) >> return ts)
-}
{-
prettyTarget :: (G.SrcPkgName, Relations, [G.BinPkgName]) -> Doc
prettyTarget (src, relss, _bins) = cat (intersperse (text ", ")  (map (prettyRels src) relss))

prettyRels :: G.SrcPkgName -> [Relation] -> Doc
prettyRels src rels =             cat (intersperse (text " | ") (map (\ rel -> cat [prettySrcPkgName src, prettyRelation rel]) rels))

prettySrcPkgName :: G.SrcPkgName -> Doc
prettySrcPkgName (G.SrcPkgName pkgname) = text pkgname
-}
{-
prettyPkgVersion :: PkgVersion -> Doc
prettyPkgVersion v = cat [text (getName v ++ "-"), prettyVersion (getVersion v)]

instance Show Target where
    show target = show . tgt $ target
-}

{-
-- |Retrieve the dependency information for a list of targets
getDependencyInfo :: Relations -> [Target] -> IO [Target]
getDependencyInfo globalBuildDeps targets =
    mapM (getTargetDependencyInfo globalBuildDeps) (cleanSource targets) >>=
    finish . partitionEithers . zipEithers targets . map eff
    where
      finish ([], ok) = return (map (\ (target, deps) -> target {targetDepends = deps}) ok)
      finish (bad, ok) =
          do -- FIXME: Any errors here should be fatal
             qPutStrLn ("Unable to retrieve build dependency info for some targets:\n  " ++
                           concat (intersperse "\n  " (map (\ (target, message) -> targetName target ++ ": " ++ message) bad)))
             return (map (\ (target, deps) -> target {targetDepends = deps}) ok)
-}

getRelaxedDependencyInfo :: Relations -> G.RelaxInfo -> DebianBuildTree -> IO (Failing (G.DepInfo, G.DepInfo))
getRelaxedDependencyInfo globalBuildDeps relaxInfo tree =
    do deps <- getTargetDependencyInfo globalBuildDeps tree
       failing (return . Failure) rdeps deps
    where rdeps deps = return . Success $ (deps, head (G.relaxDeps relaxInfo [deps]))

targetRelaxed :: G.RelaxInfo -> Target -> G.DepInfo
targetRelaxed relaxInfo target = head $ G.relaxDeps relaxInfo [targetDepends target]

-- |Retrieve the dependency information for a single target
getTargetDependencyInfo :: Relations -> DebianBuildTree -> IO (Failing G.DepInfo)
getTargetDependencyInfo globalBuildDeps buildTree =
    parseControlFromFile controlPath >>= return . either (Left . show) (G.buildDependencies . removeCommentParagraphs) >>=
    return . either (Failure . (: [])) (\ deps -> Success (addRelations globalBuildDeps deps))
    where
      controlPath = debdir buildTree ++ "/debian/control"
      addRelations :: Relations -> G.DepInfo -> G.DepInfo
      addRelations moreRels (name, relations, bins) = (name, relations ++ moreRels, bins)

{-
zipEithers :: [a] -> [Either b c] -> [Either (a, b) (a, c)]
zipEithers xs ys = 
    map zipEither (zip xs ys)
    where
      zipEither :: (a, Either b c) -> Either (a, b) (a, c)
      zipEither (x, (Left y)) = Left (x, y)
      zipEither (x, (Right y)) = Right (x, y)

-- Failing From Either - use during conversion
-- ffe :: IO a -> IO (Failing a)
-- ffe a = try a >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success

eff (Failure ss) = Left (intercalate "\n" ss)
eff (Success x) = Right x
-}
