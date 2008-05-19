-- | A Mercurial archive.
module BuildTarget.Hg where

import Debian.Shell
import Debian.Repo

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import System.Directory
import System.Exit
import System.IO
import System.Process
import System.Unix.Directory
import System.Unix.FilePath
import BuildTarget
import Extra.TIO

data Hg = Hg String SourceTree

instance Show Hg where
    show (Hg s _) = "hg:" ++ s

documentation = [ "hg:<string> - A target of this form target obtains the source"
                , "code by running the Mercurial command 'hg clone <string>'." ]

instance BuildTarget Hg where
    getTop (Hg _ tree) = topdir tree
    --getSourceTree (Hg _ tree) = tree
    --setSpecTree (Hg s _) tree = Hg s tree

    revision (Hg _ tree) =
        do (_, outh, _, handle) <- lift $ runInteractiveCommand cmd
           revision <- lift (hGetContents outh) >>= return . listToMaybe . lines >>=
                       return . maybe (Left $ "no revision info printed by '" ++ cmd ++ "'") Right
           result <- lift (try (waitForProcess handle))
           case (revision, result) of
             (Right revision, Right ExitSuccess) -> return . Right $ "hg:" ++ revision
             (Right _, Right (ExitFailure _)) -> return . Left $ "FAILURE: " ++ cmd	-- return . Right $ "hg:" ++ revision
             (Left message, _) -> return . Left $ message
             (_, Left e) -> return . Left . show $ e
        where
          path = topdir tree
          cmd = "cd " ++ outsidePath path ++ " && hg log -r $(hg id | cut -d' ' -f1 )"
    cleanTarget (Hg _ _) path =
        timeTaskAndTest (cleanStyle path (commandTask cmd))
        where
          cmd = "rm -rf " ++ outsidePath path ++ "/.hg"
          cleanStyle path = setStart (Just ("Clean Hg target in " ++ outsidePath path))

    logText (Hg _ _) revision = "Hg revision: " ++ maybe "none" id revision

prepareHg :: Bool -> FilePath -> Bool -> String -> TIO (Either String Tgt)
prepareHg _debug top flush archive =
    do
      when flush (lift $ removeRecursiveSafely dir)
      exists <- lift $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      case tree of
        Left message -> return . Left $ "Failed to find HG source tree at " ++ show dir ++ ": " ++ message
        Right tree -> return . Right . Tgt $ Hg archive tree
    where
      verifySource dir =
          runTaskAndTest (verifyStyle (commandTask ("cd " ++ dir ++ " && hg status | grep -q ."))) >>=
          either (\ _ -> updateSource dir)	-- failure means there were no changes
                 (\ _ -> removeSource dir >> createSource dir)	-- success means there was a change

      removeSource dir = lift $ removeRecursiveSafely dir

      updateSource dir =
          runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && hg pull -u"))) >>=
          either (return . Left) (const (findSourceTree (rootEnvPath dir)))
            

      createSource dir =
          let (parent, _) = splitFileName dir in
          lift (try (createDirectoryIfMissing True parent)) >>=
          either (return . Left . show) (const (runTaskAndTest (createStyle (commandTask ("hg clone " ++ archive ++ " " ++ dir))))) >>=
          either (return . Left) (const (findSourceTree (rootEnvPath dir)))

      verifyStyle = (setStart (Just ("Verifying Hg source archive " ++ archive)) .
                     setError (Just (\ _ -> "tla changes failed in" ++ show dir)))
      updateStyle = (setStart (Just ("Updating Hg source for " ++ archive)) .
                     setError (Just (\ _ -> "Update Hg Source failed in " ++ show dir)))
      createStyle = (setStart (Just ("Retrieving Hg source for " ++ archive)) .
                     setError (Just (\ _ -> "hg clone failed in " ++ show dir)))
      dir = top ++ "/hg/" ++ archive

