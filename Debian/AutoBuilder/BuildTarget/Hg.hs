{-# LANGUAGE ScopedTypeVariables #-}
-- | A Mercurial archive.
module Debian.AutoBuilder.BuildTarget.Hg where

--import Debian.OldShell (timeTaskAndTest, commandTask, setStart, runTaskAndTest, setError, runTask)
import Debian.Repo

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Lazy.Char8 (empty)
import Data.Maybe
import System.Directory
import System.Exit
import System.FilePath (splitFileName)
import System.IO
import System.Process
import System.Unix.Directory
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import System.Unix.Progress (lazyCommandF, timeTask)

data Hg = Hg String SourceTree R.RetrieveMethod

documentation = [ "hg:<string> - A target of this form target obtains the source"
                , "code by running the Mercurial command 'hg clone <string>'." ]

instance BuildTarget Hg where
    method (Hg _ _ m) = m
    getTop _ (Hg _ tree _) = topdir tree
    --getSourceTree (Hg _ tree) = tree
    --setSpecTree (Hg s _) tree = Hg s tree

    revision _ (Hg _ tree _) =
        do (_, outh, _, handle) <- liftIO $ runInteractiveCommand cmd
           rev <- hSetBinaryMode outh True >> hGetContents outh >>= return . listToMaybe . lines >>=
                  return . maybe (fail $ "no revision info printed by '" ++ cmd ++ "'") id
           result <- waitForProcess handle
           case (rev, result) of
             (rev', ExitSuccess) -> return $ "hg:" ++ rev'
             (_, ExitFailure _) -> fail $ "FAILURE: " ++ cmd	-- return . Right $ "hg:" ++ revision
        where
          path = topdir tree
          cmd = "cd " ++ path ++ " && hg log -r $(hg id | cut -d' ' -f1 )"
    cleanTarget _ (Hg _ _ _) path =
        -- timeTaskAndTest (cleanStyle path (commandTask cmd))
        timeTask (lazyCommandF cmd empty)
        where
          cmd = "rm -rf " ++ path ++ "/.hg"
          -- cleanStyle path = setStart (Just ("Clean Hg target in " ++ path))

    logText (Hg _ _ _) revision = "Hg revision: " ++ either show id revision

prepare :: P.CacheRec -> String -> R.RetrieveMethod -> AptIOT IO Hg
prepare cache archive m = liftIO $
    do
      when (P.flushSource (P.params cache)) (liftIO $ removeRecursiveSafely dir)
      exists <- liftIO $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      return $ Hg archive tree m
    where
      verifySource dir =
          -- try (runTaskAndTest (verifyStyle (commandTask ("cd " ++ dir ++ " && hg status | grep -q .")))) >>=
          try (lazyCommandF ("cd " ++ dir ++ " && hg status | grep -q .") empty) >>=
          either (\ (_ :: SomeException) -> updateSource dir)	-- failure means there were no changes
                 (\ _ -> removeSource dir >> createSource dir)	-- success means there was a change

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          -- runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && hg pull -u"))) >>
          lazyCommandF ("cd " ++ dir ++ " && hg pull -u") empty >>
          findSourceTree dir
            

      createSource dir =
          let (parent, _) = splitFileName dir in
          liftIO (createDirectoryIfMissing True parent) >>
          -- runTaskAndTest (createStyle (commandTask ("hg clone " ++ archive ++ " " ++ dir))) >>
          lazyCommandF ("hg clone " ++ archive ++ " " ++ dir) empty >>
          findSourceTree dir

      -- verifyStyle = (setStart (Just ("Verifying Hg source archive " ++ archive)) .
      --                setError (Just (\ _ -> "tla changes failed in" ++ show dir)))
      -- updateStyle = (setStart (Just ("Updating Hg source for " ++ archive)) .
      --                setError (Just (\ _ -> "Update Hg Source failed in " ++ show dir)))
      -- createStyle = (setStart (Just ("Retrieving Hg source for " ++ archive)) .
      --                setError (Just (\ _ -> "hg clone failed in " ++ show dir)))
      dir = P.topDir cache ++ "/hg/" ++ archive

