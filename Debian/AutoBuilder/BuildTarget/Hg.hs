{-# LANGUAGE ScopedTypeVariables #-}
-- | A Mercurial archive.
module Debian.AutoBuilder.BuildTarget.Hg where

import Debian.Shell
import Debian.Repo

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import System.Directory
import System.Exit
import System.FilePath (splitFileName)
import System.IO
import System.Process
import System.Unix.Directory
import Debian.AutoBuilder.BuildTarget
import Debian.AutoBuilder.ParamClass (RunClass)
import qualified Debian.AutoBuilder.ParamClass as P

data Hg = Hg String SourceTree

instance Show Hg where
    show (Hg s _) = "hg:" ++ s

documentation = [ "hg:<string> - A target of this form target obtains the source"
                , "code by running the Mercurial command 'hg clone <string>'." ]

instance BuildTarget Hg where
    getTop _ (Hg _ tree) = topdir tree
    --getSourceTree (Hg _ tree) = tree
    --setSpecTree (Hg s _) tree = Hg s tree

    revision _ (Hg _ tree) =
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
    cleanTarget _ (Hg _ _) path =
        timeTaskAndTest (cleanStyle path (commandTask cmd))
        where
          cmd = "rm -rf " ++ path ++ "/.hg"
          cleanStyle path = setStart (Just ("Clean Hg target in " ++ path))

    logText (Hg _ _) revision = "Hg revision: " ++ maybe "none" id revision

prepareHg :: (RunClass p) => p -> String -> IO Tgt
prepareHg params archive =
    do
      when (P.flushSource params) (liftIO $ removeRecursiveSafely dir)
      exists <- liftIO $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      return . Tgt $ Hg archive tree
    where
      verifySource dir =
          try (runTaskAndTest (verifyStyle (commandTask ("cd " ++ dir ++ " && hg status | grep -q .")))) >>=
          either (\ (_ :: SomeException) -> updateSource dir)	-- failure means there were no changes
                 (\ _ -> removeSource dir >> createSource dir)	-- success means there was a change

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && hg pull -u"))) >>
          findSourceTree dir
            

      createSource dir =
          createDirectoryIfMissing True (fst (splitFileName dir)) >>
          runTaskAndTest (createStyle (commandTask ("hg clone " ++ archive ++ " " ++ dir))) >>
          findSourceTree dir

      verifyStyle = (setStart (Just ("Verifying Hg source archive " ++ archive)) .
                     setError (Just (\ _ -> "tla changes failed in" ++ show dir)))
      updateStyle = (setStart (Just ("Updating Hg source for " ++ archive)) .
                     setError (Just (\ _ -> "Update Hg Source failed in " ++ show dir)))
      createStyle = (setStart (Just ("Retrieving Hg source for " ++ archive)) .
                     setError (Just (\ _ -> "hg clone failed in " ++ show dir)))
      dir = P.topDir params ++ "/hg/" ++ archive

