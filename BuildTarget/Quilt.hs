-- | The quilt target takes two other targets, one a base source
-- directory and another a quilt-style patch directory, and creates
-- a build target with the patches applied to the source directory.
module BuildTarget.Quilt where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.LocalTime
import System.Directory
import System.Exit
import Debian.Version
import BuildTarget
import Debian.IO
import Debian.Types
import Debian.Types.SourceTree
import DryRun
import Extra.List
--import Debian.Time(parseTimeRFC822)
import Text.Regex
import Debian.Local.Changes

import qualified System.IO as IO

data Quilt = Quilt Tgt Tgt SourceTree

instance Show Quilt where
    show (Quilt t q _) = "quilt:(" ++ show t ++ "):(" ++ show q ++ ")"

documentation = [ "quilt:(<target1>):(<target2>) - In a target of this form, target1 is"
                , "any source tree, and target2 is a quilt directory which contains"
                , "a debian style changelog file named 'changelog', a file named"
                , "'series' with a list of patch file names, and finally the patch"
                , "files listed in the series file.  The quilt system is used to apply"
                , "the patches to the source tree before building." ]

{-
instance Eq Quilt where
    Quilt t q _ == Quilt t' q' _ = t == t' && q == q'
-}

data EntryType = Base ChangeLogEntry | Patch ChangeLogEntry

getEntry (Base x) = x
getEntry (Patch x) = x

instance BuildTarget Quilt where
    getTop (Quilt _ _ tree) = topdir tree
    cleanTarget (Quilt (Tgt base) _ _) source = cleanTarget base source
    -- A quilt revision string is the base target revision string and the
    -- patch target revision string connected with a '+'.  If the base
    -- target has no revision string the patch revision string is used.
    revision (Quilt (Tgt base) (Tgt patch) _) =
        do
          baseRev <- BuildTarget.revision base
          case baseRev of
            Just rev ->
                do
                  patchRev <- BuildTarget.revision patch
                  return $ fmap (\ x -> "quilt:(" ++ rev ++ "):(" ++ x ++ ")") patchRev
            Nothing ->
                do tree <- findDebianSourceTree (getTop base)
                   let rev = logVersion . entry $ maybe (error $ "Invalid debian source tree") id tree
                   patchRev <- BuildTarget.revision patch
                   return $ fmap (\ x -> "quilt:(" ++ show rev ++ "):(" ++ x ++ ")") patchRev

    logText (Quilt _ _ _) revision = "Quilt revision " ++ maybe "none" id revision

quiltPatchesDir = "quilt-patches"

makeQuiltTree top base patch =
    do vPutStrLn 0 $ "Quilt base: " ++ show (getTop base)
       vPutStrLn 0 $ "Quilt patch: " ++ show (getTop patch)
       -- This will be the top directory of the quilt target
       let copyDir = rootEnvPath (top ++ "/quilt/" ++ escapeForMake ("quilt:(" ++ show base ++ "):(" ++ show patch ++ ")"))
       createDirectoryIfMissingDR True (top ++ "/quilt")
       baseTree <- findSourceTree (getTop base) >>= return . maybe (error $ "Invalid source tree: " ++ show (getTop base)) id
       copyTree <- copySourceTree baseTree copyDir
       -- If this is already a DebianBuildTree we need to apply
       -- the patch to the subdirectory containing the DebianSourceTree.
       debTree <- findOneDebianBuildTree copyDir
       -- Compute the directory where the patches will be applied
       let quiltDir = maybe copyDir debdir debTree
       patchTree <- findSourceTree (getTop patch) >>= return . maybe (error $ "Invalid source tree: " ++ show (getTop base)) id
       let patchDir = topdir patchTree
       -- Set up links to the quilt directory, and use quilt to get a
       -- list of the unapplied patches.
       let cmd1 = ("set -x && cd '" ++ outsidePath quiltDir ++ "' && rm -f '" ++ quiltPatchesDir ++
                   "' && ln -s '" ++ outsidePath patchDir ++ "' '" ++ quiltPatchesDir ++ "'")
       result <- linkStyle $ systemTask'_ cmd1
       case result of
         (ExitFailure _, _) -> error ("Failed to set up quilt target: " ++ show result)
         _ -> return ()
       -- Now we need to have created a DebianSourceTree so
       -- that there is a changelog for us to reconstruct.
       quiltTree <- findSourceTree copyDir >>= return . maybe (error $ "Failed to create source tree at " ++ show copyDir) id
       return (copyTree, quiltDir)
    where
      linkStyle = setStyle . vStyle 1 $ setStart (Just "Linking to quilt target")

prepareQuilt :: FilePath -> Bool -> Tgt -> Tgt -> AptIO Tgt
prepareQuilt top _flush (Tgt base) (Tgt patch) = 
    do (quiltTree, quiltDir) <- makeQuiltTree top base patch
       let cmd1a = ("export QUILT_PATCHES=" ++ quiltPatchesDir ++ " && cd '" ++ outsidePath quiltDir ++ "' && quilt applied")
       applied <- queryStyle1 $ systemTask' cmd1a
       case applied of
         (ExitFailure 1, s, _)
             | eConcat s == "No patches applied\n" ->
                 do
                   let cmd1b = ("export QUILT_PATCHES=" ++ quiltPatchesDir ++ " && cd '" ++ outsidePath quiltDir ++ "' && quilt unapplied")
                   unapplied <- queryStyle2 $ systemTask' cmd1b
                   --ePutStrLn ("unapplied: " ++ show unapplied)
                   let patches =
                           case unapplied of
                             (ExitSuccess, text, _) -> lines . oConcat $ text
                             _ -> error "No patches to apply"
                   -- Now apply all the unapplied patches, which should be all of
                   -- the patches.  This somewhat roundabout two step process is
                   -- required to make sure we get an error result if any of the
                   -- patches fail.
                   let cmd2 = ("export QUILT_PATCHES=" ++ quiltPatchesDir ++
                               " && cd '" ++ outsidePath quiltDir ++ "' && " ++
                               consperse " && " (map ("quilt -v --leave-reject push " ++) patches))
                   --ePutStrLn cmd2
                   result2 <- applyStyle $ systemTask'_ cmd2
                   case result2 of
                     (ExitFailure _, _) -> error $ "Failed to apply quilt patches: " ++ cmd2
                     (ExitSuccess, _) -> return ()
                   -- If there is a changelog file in the quilt directory,
                   -- interleave its entries with those in changelog of the base
                   -- tree by date.
                   io (doesFileExist (outsidePath quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog") >>=
                          (flip unless) (error ("Missing changelog file: " ++ show (outsidePath quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog"))))
                   io (mergeChangelogs (outsidePath quiltDir ++ "/debian/changelog") (outsidePath quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog"))
                   -- Return the target.
                   let cmd3 = ("cd '" ++ outsidePath quiltDir ++ "' && " ++
                               "rm -rf '" ++ outsidePath quiltDir ++ "/.pc' '" ++ outsidePath quiltDir ++ "/" ++ quiltPatchesDir ++ "'")
                   result3 <- cleanStyle $ systemTask'_ cmd3
                   case result3 of
                     (ExitFailure _, _) -> error $ "Failure removing quilt directory: " ++ cmd3
                     (ExitSuccess, _) -> return ()
		   -- Re-read the build tree with the new changelog
                   tree <- findSourceTree (topdir quiltTree) >>= return . maybe (error "Failed to find tree") id
                   return . Tgt $ Quilt (Tgt base) (Tgt patch) tree
         (ExitFailure _, s, _) -> error ("Unexpected output from quilt applied: " ++ show (eConcat s))
         (ExitSuccess, _, _) -> error "Unexpected result code from quilt applied"
    where
      queryStyle1 = setStyle . vStyle 1 $ setStart (Just "Checking for applied patches") . quietStyle IO.stderr
      queryStyle2 = setStyle . vStyle 1 $ setStart (Just "Checking for unapplied patches")
      applyStyle = setStyle . vStyle 1 $ setStart (Just "Patching Quilt target")
      cleanStyle = setStyle . vStyle 2 $ setStart (Just "Cleaning Quilt target")

--myParseTimeRFC822 x = maybe (error ("Invalid time string: " ++ show x)) id . parseTimeRFC822 $ x

-- Merge the entries in the patch changelog into the base changelog,
-- merging the base and patch version numbers as we go.  It is
-- important that we read the base changelog lazily since there are
-- lots of bizarre formats in the older entries that we can't parse.
mergeChangelogs :: FilePath -> FilePath -> IO ()
mergeChangelogs basePath patchPath =
    do
      patchEntries <- readFile patchPath >>= return . map Patch . parseLog
      -- FIXME: the changelog dates should already be parsed here
      let oldest = zonedTimeToUTC . myParseTimeRFC822 . logDate . getEntry . head . reverse $ patchEntries
      -- Parse the base changelog to the point where the oldest entry
      -- is older than the oldest patch changelog entry.
      (baseEntries, baseText') <- readFile basePath >>= return . partitionChangelog oldest
      let baseEntries' = map Base baseEntries
      let mergedEntries = third . appendVersionNumbers . sortBy compareDate $ baseEntries' ++ patchEntries
      let newText = (concat . map show $ mergedEntries) ++ baseText'
      replaceFile basePath newText
    where
      third (_, _, c) = c
      compareDate a b = compare (zonedTimeToUTC . myParseTimeRFC822 . getDate $ a) (zonedTimeToUTC . myParseTimeRFC822 . getDate $ b)
      getDate (Base entry) = logDate entry
      getDate (Patch entry) = logDate entry
      -- The version numbers of the patch entries need to be prefixed
      -- with the previous base version.  The base version numbers
      -- need to be suffixed with the previous patch version number.
      appendVersionNumbers entries = foldl modifyVersion (Nothing, Nothing, []) entries
      modifyVersion :: (Maybe DebianVersion, Maybe DebianVersion, [ChangeLogEntry]) -> EntryType
                    -> (Maybe DebianVersion, Maybe DebianVersion, [ChangeLogEntry])
      -- A base entry before the first patch entry, do nothing
      modifyVersion (_, Nothing, modified) (Base entry) = (Just (logVersion entry), Nothing, entry : modified)
      -- A patch entry before the first base entry, an error
      modifyVersion (Nothing, _, _modified) (Patch _entry) =
          error "Patch changelog entry is older than older than oldest base entry"
      -- Prefix a patch entry with the base version
      modifyVersion (Just baseVersion, _, modified) (Patch entry) =
          (Just baseVersion, (Just . logVersion $ entry), (newEntry : modified))
          where newEntry = entry {logVersion = buildQuiltVersion baseVersion (logVersion entry)}
      -- Suffix a base entry with the patch version
      modifyVersion (_, Just patchVersion, modified) (Base entry) =
          ((Just . logVersion $ entry), Just patchVersion, (newEntry : modified))
          where newEntry = entry {logVersion = buildQuiltVersion (logVersion entry) patchVersion}
      buildQuiltVersion baseVersion patchVersion =
          case Debian.Version.revision baseVersion of
            Just _ -> parseDebianVersion (show baseVersion ++ "+" ++ show patchVersion)
            Nothing -> parseDebianVersion (show baseVersion ++ "-" ++ show patchVersion)

partitionChangelog date text =
    case parseEntry text of
      Nothing -> ([], text)
      Just (entry, text') ->
          if date >= (zonedTimeToUTC . myParseTimeRFC822 . logDate $ entry)
          then ([entry], text')
          else case partitionChangelog date text' of
                 (entries, text'') -> (entry : entries, text'')

replaceFile path text =
    doesFileExist path >>= (flip when) (removeFile path) >> writeFile path text

-- |This function is a bit less stringent than the official one -
-- e.g., it will accept "Wed, 10 Oct 2007 06:00:57 +0000", which the
-- official function won't.
myParseTimeRFC822 s =
    case matchRegex (mkRegex "^..., (..) (...) (....) (..):(..):(..) (.)(..)(..)$") s of
      Just [dom, mon, year, hour, min, sec, zoneSign, zoneHours, zoneMinutes] ->
          ZonedTime (localTime dom mon year hour min sec) (timeZone zoneSign zoneHours zoneMinutes)
      _ -> error ("Invalid date string: " ++ s)
    where
      -- spaceToZero ' ' = '0'
      -- spaceToZero x = x
      localTime dom mon year hr min sec = LocalTime (localDay dom mon year) (timeOfDay hr min sec)
      localDay dom mon year = fromGregorian (read year) (monthNumber mon) (read dom)
      timeZone "+" zoneHours zoneMinutes = TimeZone (60 * read zoneHours + read zoneMinutes) False ""
      timeZone "-" zoneHours zoneMinutes = TimeZone (- (60 * read zoneHours + read zoneMinutes)) False ""
      timeZone _ _ _ = error ("Time string has invalid time zone: " ++ s)
      timeOfDay hr min sec = TimeOfDay (read hr) (read min) (fromInteger . read $ sec)
      monthNumber "Jan" = 1
      monthNumber "Feb" = 2
      monthNumber "Mar" = 3
      monthNumber "Apr" = 4
      monthNumber "May" = 5
      monthNumber "Jun" = 6
      monthNumber "Jul" = 7
      monthNumber "Aug" = 8
      monthNumber "Sep" = 9
      monthNumber "Oct" = 10
      monthNumber "Nov" = 11
      monthNumber "Dec" = 12
      monthNumber _ = error ("Invalid month in time string: " ++ s)
