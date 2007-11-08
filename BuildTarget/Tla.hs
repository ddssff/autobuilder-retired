module BuildTarget.Tla where

import BuildTarget
import Debian.SourceTree
import Debian.Types
import System.IO
import Control.Monad
import System.Process
import Linspire.Unix.Directory
import Linspire.Unix.FilePath
import Data.Maybe
import System.Exit
import System.Directory
import Debian.IO
import DryRun

-- | A TLA archive
data Tla = Tla String SourceTree

instance Show Tla where
    show (Tla s _) = "tla:" ++ s

instance BuildTarget Tla where
    getTop (Tla _ tree) = dir tree
    cleanTarget (Tla _ _) source =
        do let path = dir source
               cmd = "find '" ++ outsidePath path ++ "' -name '.arch-ids' -o -name '{arch}' -prune | xargs rm -rf"
           cleanStyle path $ systemTask_ cmd
           return ()
        where cleanStyle path = setStyle $ setStart (Just ("Clean TLA target in " ++ outsidePath path))

    revision (Tla _ tree) =
        do let path = dir tree
               cmd = "cd " ++ outsidePath path ++ " && tla revisions -f -r | head -1"
           -- FIXME: this command can take a lot of time, message it
           (_, outh, _, handle) <- io $ runInteractiveCommand cmd
           revision <- io (hGetContents outh >>= return . listToMaybe . lines) >>=
                       return . maybe (error "no revision info printed by '" ++ cmd ++ "'") id
           io $ waitForProcess handle
           return . Just $ "tla:" ++ revision

    logText (Tla _ _) revision = "TLA revision: " ++ maybe "none" id revision

prepareTla :: FilePath -> Bool -> String -> AptIO Tgt
prepareTla top flush version =
    do
      when flush (removeRecursiveSafelyDR dir)
      exists <- io $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      case tree of
        Nothing -> error ("failed to find source tree at " ++ dir)
        Just tree -> return $ Tgt $ Tla version tree
    where
      verifySource dir =
          do result <- verifyStyle $ systemTask'_ ("cd " ++ dir ++ " && tla changes")
             case result of
               (ExitSuccess, _) -> updateSource dir				-- Success means no changes
               (ExitFailure _, _) -> removeSource dir >> createSource dir	-- Failure means there is corruption

      removeSource dir = io $ removeRecursiveSafely dir

      updateSource dir =
          do updateStyle $ systemTask_ ("cd " ++ dir ++ " && tla update " ++ version)
             -- At one point we did a tla undo here.  However, we are
             -- going to assume that the "clean" copies in the cache
             -- directory are clean, since some of the other target
             -- types have no way of doing this reversion.
             findSourceTree (rootEnvPath dir)

      createSource dir =
          do
            -- Create parent dir and let tla create dir
            let (parent, _) = splitFileName dir
            io $ createDirectoryIfMissing True parent
            createStyle $ systemTask ("tla get " ++ version ++ " " ++ dir)
            findSourceTree (rootEnvPath dir)

      verifyStyle = setStyle (setStart (Just ("Verifying TLA source archive " ++ version)) .
                              setError (Just ("tla changes failed in" ++ dir)) {- . Output Indented-})
      updateStyle = setStyle (setStart (Just ("Updating TLA source for " ++ version)) .
                              setError (Just "updateSource failed") {- . Output Indented -})
      createStyle = setStyle (setStart (Just ("Retrieving TLA source for " ++ version)) .
                              setError (Just ("tla get failed in " ++ dir)) .
                              setEcho True)
      dir = top ++ "/tla/" ++ version
