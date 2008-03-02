module BuildTarget.Tla where

import Control.Monad.Trans
import BuildTarget
import Debian.Types
import Debian.Types.SourceTree
import System.IO
import Control.Monad
import System.Process
import System.Unix.Directory
import System.Unix.FilePath
import Data.Maybe
import System.Directory
import Extra.TIO
import Debian.Shell

-- | A TLA archive
data Tla = Tla String SourceTree

instance Show Tla where
    show (Tla s _) = "tla:" ++ s

documentation = [ "tla:<revision> - A target of this form retrieves the a TLA archive with the"
                , "given revision name." ]

instance BuildTarget Tla where
    getTop (Tla _ tree) = topdir tree
    cleanTarget (Tla _ _) path =
        timeTaskAndTest (cleanStyle path (commandTask cmd))
        where
          cmd = "find '" ++ outsidePath path ++ "' -name '.arch-ids' -o -name '{arch}' -prune | xargs rm -rf"
          cleanStyle path = setStart (Just ("Clean TLA target in " ++ outsidePath path))

    revision (Tla _ tree) =
        do let path = topdir tree
               cmd = "cd " ++ outsidePath path ++ " && tla revisions -f -r | head -1"
           -- FIXME: this command can take a lot of time, message it
           (_, outh, _, handle) <- lift $ runInteractiveCommand cmd
           revision <- lift (hGetContents outh >>= return . listToMaybe . lines) >>=
                       return . maybe (error "no revision info printed by '" ++ cmd ++ "'") id
           lift $ waitForProcess handle
           return . Right $ "tla:" ++ revision

    logText (Tla _ _) revision = "TLA revision: " ++ maybe "none" id revision

prepareTla :: FilePath -> Bool -> String -> TIO (Either String Tgt)
prepareTla top flush version =
    do
      when flush (lift (removeRecursiveSafely dir))
      exists <- lift $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      case tree of
        Left message -> return . Left $ "failed to find source tree at " ++ dir ++ ": " ++ message
        Right tree -> return . Right . Tgt $ Tla version tree
    where
      verifySource dir =
          do result <- runTaskAndTest (verifyStyle (commandTask ("cd " ++ dir ++ " && tla changes")))
             case result of
               Left message -> vPutStrBl 0 message >> removeSource dir >> createSource dir	-- Failure means there is corruption
               Right output -> updateSource dir						-- Success means no changes

      removeSource dir = lift $ removeRecursiveSafely dir

      updateSource dir =
          runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && tla update " ++ version))) >>=
             -- At one point we did a tla undo here.  However, we are
             -- going to assume that the "clean" copies in the cache
             -- directory are clean, since some of the other target
             -- types have no way of doing this reversion.
          either (return . Left) (const (findSourceTree (rootEnvPath dir)))

      createSource dir =
          do
            -- Create parent dir and let tla create dir
            let (parent, _) = splitFileName dir
            lift $ createDirectoryIfMissing True parent
            runTaskAndTest (createStyle (commandTask ("tla get " ++ version ++ " " ++ dir)))
            findSourceTree (rootEnvPath dir)

      verifyStyle = (setStart (Just ("Verifying TLA source archive " ++ version)) .
                     setError (Just (\ _ -> "tla changes failed in" ++ dir)) {- . Output Indented-})
      updateStyle = (setStart (Just ("Updating TLA source for " ++ version)) .
                     setError (Just (\ _ -> "updateSource failed")) {- . Output Indented -})
      createStyle = (setStart (Just ("Retrieving TLA source for " ++ version)) .
                     setError (Just (\ _ -> "tla get failed in " ++ dir)))
      dir = top ++ "/tla/" ++ version
