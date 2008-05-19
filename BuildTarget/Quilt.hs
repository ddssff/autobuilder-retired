-- | The quilt target takes two other targets, one a base source
-- directory and another a quilt-style patch directory, and creates
-- a build target with the patches applied to the source directory.
module BuildTarget.Quilt where

import Debian.Repo.Changes
import Debian.Repo.SourceTree
import Debian.Repo.Types
import Debian.Shell
import Debian.Version

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.LocalTime
import Extra.TIO
import Extra.Either
import Extra.Files
import Extra.List
import System.Directory
import System.Exit
import BuildTarget
import System.Unix.Process
--import Debian.Time(parseTimeRFC822)
import Text.Regex

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
        do baseRev <- BuildTarget.revision base
           case baseRev of
             Right rev ->
                 do patchRev <- BuildTarget.revision patch
                    return $ fmap (\ x -> "quilt:(" ++ rev ++ "):(" ++ x ++ ")") patchRev
             Left message ->
                 do tree <- findDebianSourceTree (getTop base)
                    let rev = logVersion . entry $ either (const (error $ "Invalid debian source tree")) id tree
                    patchRev <- BuildTarget.revision patch
                    return $ fmap (\ x -> "quilt:(" ++ show rev ++ "):(" ++ x ++ ")") patchRev

    logText (Quilt _ _ _) revision = "Quilt revision " ++ maybe "none" id revision

quiltPatchesDir = "quilt-patches"

makeQuiltTree :: (Show a, Show b, BuildTarget a, BuildTarget b) => FilePath -> a -> b -> TIO (Either String (SourceTree, EnvPath))
makeQuiltTree top base patch =
    do vPutStrBl 1 $ "Quilt base: " ++ outsidePath (getTop base)
       vPutStrBl 1 $ "Quilt patch: " ++ outsidePath (getTop patch)
       -- This will be the top directory of the quilt target
       let copyDir = rootEnvPath (top ++ "/quilt/" ++ escapeForMake ("quilt:(" ++ show base ++ "):(" ++ show patch ++ ")"))
       lift (createDirectoryIfMissing True (top ++ "/quilt"))
       baseTree <- findSourceTree (getTop base) >>=
                   return . either (\ message -> Left $ "Invalid source tree " ++ show (getTop base) ++ ": " ++ message) Right
       patchTree <- findSourceTree (getTop patch) >>=
                    return . either (\ message -> Left $ "Invalid source tree " ++ show (getTop base) ++ ": " ++ message) Right
       case (baseTree, patchTree) of
         (Right baseTree, Right patchTree) ->
             do copyTree <- copySourceTree baseTree copyDir
                case copyTree of
                  Left message -> return (Left message)
                  Right copyTree ->
                      do -- If this is already a DebianBuildTree we need to apply
                         -- the patch to the subdirectory containing the DebianSourceTree.
                         debTree <- findOneDebianBuildTree copyDir
                         -- Compute the directory where the patches will be applied
                         let quiltDir = maybe copyDir debdir debTree
                         vPutStrBl 2 $ "copyDir: " ++ outsidePath copyDir
                         vPutStrBl 2 $ "quiltDir: " ++ outsidePath quiltDir
                         let patchDir = topdir patchTree
                         -- Set up links to the quilt directory, and use quilt to get a
                         -- list of the unapplied patches.
                         let cmd1 = ("set -x && cd '" ++ outsidePath quiltDir ++ "' && rm -f '" ++ quiltPatchesDir ++
                                     "' && ln -s '" ++ outsidePath patchDir ++ "' '" ++ quiltPatchesDir ++ "'")
                         result <- runTaskAndTest (linkStyle (commandTask cmd1))
                         -- Now we need to have created a DebianSourceTree so
                         -- that there is a changelog for us to reconstruct.
                         case result of
                           Left message -> return . Left $ "Failed to set up quilt target " ++ show result ++ ": " ++ message
                           _ -> return (Right (copyTree, quiltDir))
         (Left message, _) -> return (Left message)
         (_, Left message) -> return (Left message)
    where
      linkStyle = setStart (Just "Linking to quilt target")

debug e =
    do IO.hPutStrLn IO.stderr ("Missed exception: " ++ show e) 
       IO.hFlush IO.stderr
       exitWith (ExitFailure 2)

prepareQuilt :: FilePath -> Bool -> Tgt -> Tgt -> TIO (Either String Tgt)
prepareQuilt top _flush (Tgt base) (Tgt patch) = 
    tryTIO (makeQuiltTree top base patch >>= either (return . Left) make) >>= either (lift . debug) return
    where
      make (quiltTree, quiltDir) =
          do applied <- lift (lazyCommand cmd1a L.empty) >>= vMessage 1 "Checking for applied patches" >>= return . collectOutputUnpacked
             case applied of
               (_, err, [ExitFailure 1])
                   | err == "No patches applied\n" ->
                          findUnapplied >>= apply >>= buildLog >>= cleanSource
                          where
                            findUnapplied = do unapplied <- lift (lazyCommand cmd1b L.empty) >>= vMessage 1 "Checking for unapplied patches" . collectOutputUnpacked
                                               case unapplied of
                                                 (text, _, [ExitSuccess]) -> return (Right (lines text))
                                                 _ -> return (Left $ target ++ " - No patches to apply")
                            apply (Left message) = return (Left message)
                            apply (Right patches) =
                                do result2 <- lift (lazyCommand (cmd2 patches) L.empty) >>= vMessage 1 "Patching Quilt target" . collectOutput . mergeToStderr
                                   case result2 of
                                     (_, _, [ExitSuccess]) -> return (Right ())
                                     (_, err, _) ->
                                         return (Left $ target ++ " - Failed to apply quilt patches: " ++ (cmd2 patches) ++ " ->\n" ++ L.unpack err)
                            buildLog (Left message) = return (Left message)
                            buildLog (Right ()) =
                                -- If there is a changelog file in the quilt directory,
                                -- interleave its entries with those in changelog of the base
                                -- tree by date.
                                do vEPutStrBl 1 "Merging changelogs"
                                   exists <- lift (doesFileExist (outsidePath quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog"))
                                   case exists of
                                     False -> return (Left (target ++ "- Missing changelog file: " ++ show (outsidePath quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog")))
                                     True -> mergeChangelogs' (outsidePath quiltDir ++ "/debian/changelog") (outsidePath quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog")
                            cleanSource (Left message) = return (Left message)
                            cleanSource (Right ()) =
                                do result3 <- lift (lazyCommand cmd3 L.empty) >>= vMessage 1 "Cleaning Quilt target" . collectOutput
                                   case result3 of
                                     (_, _, [ExitSuccess]) ->
                                         findSourceTree (topdir quiltTree) >>= return . either (const . Left $ target ++ " - Failed to find tree") (\ tree -> Right . Tgt $ Quilt (Tgt base) (Tgt patch) tree)
                                     _ -> return (Left $ target ++ " - Failure removing quilt directory: " ++ cmd3)
{-
                          do unapplied <- lift (lazyCommand cmd1b L.empty) >>= vMessage 1 "Checking for unapplied patches" . collectOutputUnpacked
                             --ePutStrLn ("unapplied: " ++ show unapplied)
                             let patches =
                                     case unapplied of
                                       (text, _, [ExitSuccess]) -> lines text
                                       _ -> error "No patches to apply"
                             --ePutStrLn cmd2
                             result2 <- lift (lazyCommand (cmd2 patches) L.empty) >>= vMessage 1 "Patching Quilt target" . collectOutput
                             case result2 of
                               (_, _, [ExitSuccess]) -> return ()
                               _ -> error $ "Failed to apply quilt patches: " ++ (cmd2 patches)
                             -- If there is a changelog file in the quilt directory,
                             -- interleave its entries with those in changelog of the base
                             -- tree by date.
                             lift (doesFileExist (outsidePath quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog") >>=
                                   (flip unless) (error ("Missing changelog file: " ++ show (outsidePath quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog"))))
                             lift (mergeChangelogs (outsidePath quiltDir ++ "/debian/changelog") (outsidePath quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog"))
                             -- Return the target.
                             result3 <- lift (lazyCommand cmd3 L.empty) >>= vMessage 1 "Cleaning Quilt target" . collectOutput
                             case result3 of
                               (_, _, [ExitSuccess]) -> return ()
                               _ -> error $ "Failure removing quilt directory: " ++ cmd3
	                     -- Re-read the build tree with the new changelog
                             findSourceTree (topdir quiltTree) >>= return . either (const . Left $ "Failed to find tree") (\ tree -> Right . Tgt $ Quilt (Tgt base) (Tgt patch) tree)
-}
               (_, err, [ExitFailure _]) -> return (Left $ target ++ " - Unexpected output from quilt applied: " ++ err)
               (_, _, [ExitSuccess]) -> return (Left $ target ++ " - Unexpected result code from quilt applied")
               (_, _, other) -> return (Left $ target ++ " - Bad result code from quilt applied process: " ++ show other)
          where
            cmd1a = ("export QUILT_PATCHES=" ++ quiltPatchesDir ++ " && cd '" ++ outsidePath quiltDir ++ "' && quilt applied")
            cmd1b = ("export QUILT_PATCHES=" ++ quiltPatchesDir ++ " && cd '" ++ outsidePath quiltDir ++ "' && quilt unapplied")
            -- Apply all the unapplied patches, which should be all of
            -- the patches.  This somewhat roundabout two step process
            -- is required to make sure we get an error result if any
            -- of the patches fail.
            cmd2 patches =
                ("export QUILT_PATCHES=" ++ quiltPatchesDir ++
                 " && cd '" ++ outsidePath quiltDir ++ "' && " ++
                 consperse " && " (map ("quilt -v --leave-reject push " ++) patches))
            cmd3 = ("cd '" ++ outsidePath quiltDir ++ "' && " ++
                    "rm -rf '" ++ outsidePath quiltDir ++ "/.pc' '" ++ outsidePath quiltDir ++ "/" ++ quiltPatchesDir ++ "'")
            target = "quilt:(" ++ show base ++ "):(" ++ show patch ++ ")"
             
--myParseTimeRFC822 x = maybe (error ("Invalid time string: " ++ show x)) id . parseTimeRFC822 $ x

mergeChangelogs' :: FilePath -> FilePath -> TIO (Either String ())
mergeChangelogs' basePath patchPath =
    do patchText <- lift (try (readFile patchPath))
       baseText <- lift (try (readFile basePath))
       case (patchText, baseText) of
         (Right patchText, Right baseText) ->
             do -- vEPutStrBl 1 $ "Merging changelogs: " ++ baseText ++ "\npatch:\n\n" ++ patchText
                either (return . Left) replace (mergeChangelogs baseText patchText)
         (Left e, _) -> return $ Left (show e)
         (_, Left e) -> return $ Left (show e)
    where
      replace newText = lift (try (replaceFile basePath $! newText)) >>= return. either (Left . show) Right

-- Merge the entries in the patch changelog into the base changelog,
-- merging the base and patch version numbers as we go.  It is
-- important that we read the base changelog lazily since there are
-- lots of bizarre formats in the older entries that we can't parse.
mergeChangelogs :: String -> String -> Either String String
mergeChangelogs baseText patchText =
    case partitionEithers (parseLog patchText) of
      (bad@(_ : _), _) ->
          Left $ "Error(s) in patch changelog:\n  " ++ concat (intersperse "\n  " bad)
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
                Right $ (concat . map show $ mergedEntries) ++ baseText'
            False ->
                Left $ "Package name mismatch between base and patch changelogs: " ++
                       maybe "?" id basePackage ++ " /= " ++ maybe "?" id patchPackage
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
            Just _ -> parseDebianVersion (show baseVersion ++ "+" ++ show patchVersion)
            Nothing -> parseDebianVersion (show baseVersion ++ "-" ++ show patchVersion)

partitionChangelog :: UTCTime -> String -> ([ChangeLogEntry], String)
partitionChangelog date text =
    case parseEntry text of
      Nothing -> ([], "")
      Just (Left _) -> ([], text)
      Just (Right (entry, text')) ->
          if date >= (zonedTimeToUTC . myParseTimeRFC822 . logDate $ entry)
          then ([entry], text')
          else case partitionChangelog date text' of
                 (entries, text'') -> (entry : entries, text'')

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
