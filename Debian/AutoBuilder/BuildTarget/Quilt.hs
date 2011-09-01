{-# LANGUAGE ScopedTypeVariables #-}
-- | The quilt target takes two other targets, one a base source
-- directory and another a quilt-style patch directory, and creates
-- a build target with the patches applied to the source directory.
module Debian.AutoBuilder.BuildTarget.Quilt where

import Debian.Changes (ChangeLogEntry(..), prettyEntry, parseLog, parseEntry)
import Debian.Repo (DebianSourceTreeC(entry, debdir), SourceTreeC(topdir), SourceTree, findSourceTree, findDebianSourceTree, findOneDebianBuildTree, copySourceTree)
--import Debian.OldShell (setStart, commandTask, runTaskAndTest)
import Debian.Version

import Control.Applicative.Error (Failing(..), failing)
import Control.Exception (SomeException, try, throw)
import Control.Monad (when)
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Either (partitionEithers)
import Data.List (intercalate, sortBy)
import Data.Maybe
import Data.Time
import Data.Time.LocalTime ()
import qualified Debian.AutoBuilder.BuildTarget.Common as BuildTarget (revision)
import Debian.AutoBuilder.BuildTarget.Common (BuildTarget(cleanTarget, logText), getTop, md5sum)
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Tgt (Tgt(Tgt))
import Debian.Repo (AptIOT)
import Extra.Files (replaceFile)
import Extra.List ()
import System.Directory (doesFileExist, createDirectoryIfMissing, doesDirectoryExist, renameDirectory)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Unix.Process (collectOutputUnpacked, collectOutput, mergeToStderr)
import System.Unix.Progress (lazyCommandF, lazyCommandE, lazyCommandV)
import System.Unix.QIO (quieter, qPutStrLn, qMessage)
--import Debian.Time(parseTimeRFC822)
import Text.Regex


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
    getTop _ (Quilt _ _ tree) = topdir tree
    cleanTarget params (Quilt base _ _) source = cleanTarget params base source
    -- A quilt revision string is the base target revision string and the
    -- patch target revision string connected with a '+'.  If the base
    -- target has no revision string the patch revision string is used.
    revision params (Quilt base patch _) =
        do baseRev <- try (BuildTarget.revision params base)
           case baseRev of
             Right (rev :: String) ->
                 BuildTarget.revision params patch >>= \ patchRev -> return ("quilt:(" ++ rev ++ "):(" ++ patchRev ++ ")")
             Left (_ :: SomeException) ->
                 do tree <- findDebianSourceTree (getTop params base)
                    let rev = logVersion . entry $ tree
                    BuildTarget.revision params patch >>= \ patchRev -> return ("quilt:(" ++ show rev ++ "):(" ++ patchRev ++ ")")

    logText (Quilt _ _ _) rev = "Quilt revision " ++ either show id rev

quiltPatchesDir = "quilt-patches"

makeQuiltTree :: (Show a, Show b, BuildTarget a, BuildTarget b) => P.CacheRec -> a -> b -> IO (SourceTree, FilePath)
makeQuiltTree cache base patch =
    do quieter 1 (qPutStrLn $ "Quilt base: " ++ getTop (P.params cache) base)
       quieter 1 (qPutStrLn $ "Quilt patch: " ++ getTop (P.params cache) patch)
       -- This will be the top directory of the quilt target
       let copyDir = P.topDir cache ++ "/quilt/" ++ md5sum ("quilt:(" ++ show base ++ "):(" ++ show patch ++ ")")
       liftIO (createDirectoryIfMissing True (P.topDir cache ++ "/quilt"))
       baseTree <- try (findSourceTree (getTop (P.params cache) base))
       patchTree <- try (findSourceTree (getTop (P.params cache) patch))
       case (baseTree, patchTree) of
         (Right baseTree, Right patchTree) ->
             do copyTree <- copySourceTree baseTree copyDir
                -- If this is already a DebianBuildTree we need to apply
                -- the patch to the subdirectory containing the DebianSourceTree.
                debTree <- findOneDebianBuildTree copyDir
                -- Compute the directory where the patches will be applied
                let quiltDir = failing (const copyDir) debdir debTree
                quieter 2 (qPutStrLn $ "copyDir: " ++ copyDir)
                quieter 2 (qPutStrLn $ "quiltDir: " ++ quiltDir)
                let patchDir = topdir patchTree
                -- Set up links to the quilt directory, and use quilt to get a
                -- list of the unapplied patches.
                let cmd1 = ("set -x && cd '" ++ quiltDir ++ "' && rm -f '" ++ quiltPatchesDir ++
                            "' && ln -s '" ++ patchDir ++ "' '" ++ quiltPatchesDir ++ "'")
                -- runTaskAndTest (linkStyle (commandTask cmd1))
                _output <- lazyCommandF cmd1 L.empty
                -- Now we need to have created a DebianSourceTree so
                -- that there is a changelog for us to reconstruct.
                return (copyTree, quiltDir)
         (Left (e :: SomeException), _) -> throw e
         (_, Left (e :: SomeException)) -> throw e
    -- where linkStyle = setStart (Just "Linking to quilt target")

{-
debug :: SomeException -> IO (Either String Tgt)
debug e =
    do IO.hPutStrLn IO.stderr ("Missed exception: " ++ s) 
       IO.hFlush IO.stderr
       exitWith (ExitFailure 2)
       return (Left s)
    where s = show e
-}

prepare :: P.CacheRec -> Tgt -> Tgt -> AptIOT IO Quilt
prepare cache base patch = liftIO $
    makeQuiltTree cache base patch >>= withUpstreamQuiltHidden make
    where
      withUpstreamQuiltHidden make (quiltTree, quiltDir) =
          hide >> make (quiltTree, quiltDir) >>= unhide
          where hide = doesDirectoryExist pc >>= (flip when) (rmrf pch >> renameDirectory pc pch)
                unhide x = doesDirectoryExist pch >>= (flip when) (rmrf pc >> renameDirectory pch pc) >> return x
                pc = (quiltDir ++ "/.pc")
                pch = (quiltDir ++ "/.pc.hide")
                rmrf d = lazyCommandV ("rm -rf '"  ++ d ++ "'") L.empty
      make :: (SourceTree, FilePath) -> IO Quilt
      make (quiltTree, quiltDir) =
          do applied <- lazyCommandE cmd1a L.empty >>= quieter 1 . qMessage "Checking for applied patches" >>= return . collectOutputUnpacked
             case applied of
               (_, err, ExitFailure 1)
                   | err == "No patches applied\n" ->
                          findUnapplied >>= apply >> buildLog >> cleanSource
                          where
                            findUnapplied = do unapplied <- liftIO (lazyCommandE cmd1b L.empty) >>= qMessage "Checking for unapplied patches" . collectOutputUnpacked
                                               case unapplied of
                                                 (text, _, ExitSuccess) -> return (lines text)
                                                 _ -> fail $ target ++ " - No patches to apply"
                            apply patches =
                                do result2 <- liftIO (lazyCommandE (cmd2 patches) L.empty) >>= qMessage "Patching Quilt target" . collectOutput . mergeToStderr
                                   case result2 of
                                     (_, _, ExitSuccess) -> return ()
                                     (_, err, _) -> fail $ target ++ " - Failed to apply quilt patches: " ++ L.unpack err
                                         -- fail $ target ++ " - Failed to apply quilt patches: " ++ (cmd2 patches) ++ " ->\n" ++ L.unpack err
                            buildLog =
                                -- If there is a changelog file in the quilt directory,
                                -- interleave its entries with those in changelog of the base
                                -- tree by date.
                                do quieter 1 (qPutStrLn "Merging changelogs")
                                   exists <- doesFileExist (quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog")
                                   case exists of
                                     False -> fail (target ++ "- Missing changelog file: " ++ show (quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog"))
                                     True -> mergeChangelogs' (quiltDir ++ "/debian/changelog") (quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog")
                            cleanSource =
                                do result3 <- liftIO (lazyCommandE cmd3 L.empty) >>= quieter 1 . qMessage "Cleaning Quilt target" . collectOutput
                                   case result3 of
                                     (_, _, ExitSuccess) ->
                                         findSourceTree (topdir quiltTree) >>= return . Quilt (Tgt base) (Tgt patch)
                                     _ -> fail $ target ++ " - Failure removing quilt directory: " ++ cmd3
               (_, err, ExitFailure _) -> fail $ target ++ " - Unexpected output from quilt applied: " ++ err
               (_, _, ExitSuccess) -> fail $ target ++ " - Unexpected result code (ExitSuccess) from " ++ show cmd1a
          where
            cmd1a = ("export QUILT_PATCHES=" ++ quiltPatchesDir ++ " && cd '" ++ quiltDir ++ "' && quilt applied")
            cmd1b = ("export QUILT_PATCHES=" ++ quiltPatchesDir ++ " && cd '" ++ quiltDir ++ "' && quilt unapplied")
            -- Apply all the unapplied patches, which should be all of
            -- the patches.  This somewhat roundabout two step process
            -- is required to make sure we get an error result if any
            -- of the patches fail.
            cmd2 patches =
                ("export QUILT_PATCHES=" ++ quiltPatchesDir ++
                 " && cd '" ++ quiltDir ++ "' && " ++
                 intercalate " && " (map ("quilt -v --leave-reject push " ++) patches))
            cmd3 = ("cd '" ++ quiltDir ++ "' && " ++
                    "rm -rf '" ++ quiltDir ++ "/.pc' '" ++ quiltDir ++ "/" ++ quiltPatchesDir ++ "'")
            target = "quilt:(" ++ show base ++ "):(" ++ show patch ++ ")"
             
--myParseTimeRFC822 x = maybe (error ("Invalid time string: " ++ show x)) id . parseTimeRFC822 $ x

mergeChangelogs' :: FilePath -> FilePath -> IO (Either String ())
mergeChangelogs' basePath patchPath =
    do patchText <- liftIO (try (readFile patchPath))
       baseText <- liftIO (try (readFile basePath))
       case (patchText, baseText) of
         (Right patchText, Right baseText) ->
             do -- vEPutStrBl 1 $ "Merging changelogs: " ++ baseText ++ "\npatch:\n\n" ++ patchText
                either (return . Left) replace (mergeChangelogs baseText patchText)
         (Left (e :: SomeException), _) -> return $ Left (show e)
         (_, Left (e :: SomeException)) -> return $ Left (show e)
    where
      replace newText = liftIO (try (replaceFile basePath $! newText)) >>=
                        return. either (\ (e :: SomeException) -> Left . show $ e) Right

partitionFailing :: [Failing a] -> ([[String]], [a])
partitionFailing [] = ([], [])
partitionFailing (x : xs) =
    f x (partitionFailing xs)
    where
      f (Failure x) (failures, successes) = (x : failures, successes)
      f (Success x) (failures, successes) = (failures, x : successes)

-- Merge the entries in the patch changelog into the base changelog,
-- merging the base and patch version numbers as we go.  It is
-- important that we read the base changelog lazily since there are
-- lots of bizarre formats in the older entries that we can't parse.
mergeChangelogs :: String -> String -> Either String String
mergeChangelogs baseText patchText =
    case partitionEithers (parseLog patchText) of
      ([], patchEntries) ->
          let patchEntries' = map Patch patchEntries in
          let oldest = zonedTimeToUTC . myParseTimeRFC822 . logDate . getEntry . head . reverse $ patchEntries' in
          let (baseEntries, baseText') = partitionChangelog oldest baseText in
          let basePackage = maybe Nothing (Just . logPackage) (listToMaybe baseEntries) in
          let patchPackage = maybe Nothing (Just . logPackage) (listToMaybe patchEntries) in
          case basePackage == patchPackage of
            True ->
                let baseEntries' = map Base baseEntries in
                let mergedEntries = third . appendVersionNumbers . sortBy compareDate $ baseEntries' ++ patchEntries' in
                Right $ (intercalate "\n\n" (map (show . prettyEntry) mergedEntries)) ++ baseText'
            False ->
                Left $ "Package name mismatch between base and patch changelogs: " ++
                       maybe "?" id basePackage ++ " /= " ++ maybe "?" id patchPackage
      (failures, _) ->
          Left $ "Error(s) in patch changelog:\n  " ++ intercalate "\n  " (concat failures)
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
      modifyVersion x@(Nothing, _, _modified) (Patch _entry) =
          -- This used to be an error:
          -- error "Patch changelog entry is older than oldest base entry"
          x
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
            Just _ -> parseDebianVersion (show baseVersion ++ "++" ++ show patchVersion)
            Nothing -> parseDebianVersion (show baseVersion ++ "-" ++ show patchVersion)

partitionChangelog :: UTCTime -> String -> ([ChangeLogEntry], String)
partitionChangelog date text =
    case parseEntry text of
      Left _msgs -> ([], text)
      Right (entry, text') ->
          if date >= (zonedTimeToUTC . myParseTimeRFC822 . logDate $ entry)
          then ([entry], text')
          else case partitionChangelog date text' of
                 (entries, text'') -> (entry : entries, text'')

-- |This function is a bit less stringent than the official one -
-- e.g., it will accept "Wed, 10 Oct 2007 06:00:57 +0000", which the
-- official function won't.
myParseTimeRFC822 s =
    case matchRegex (mkRegex "^..., (.?.) (...) (....) (..):(..):(..) (.)(..)(..)$") s of
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
