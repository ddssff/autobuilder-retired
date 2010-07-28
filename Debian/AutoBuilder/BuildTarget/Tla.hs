{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Tla where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Debian.AutoBuilder.BuildTarget
import Debian.AutoBuilder.ParamClass (RunClass)
import qualified Debian.AutoBuilder.ParamClass as P
import Debian.Repo
import Debian.Shell
import Debian.Extra.CIO
import System.FilePath (splitFileName)
import System.IO
import System.Process
import System.Unix.Directory
import System.Directory

-- | A TLA archive
data Tla = Tla String SourceTree

instance Show Tla where
    show (Tla s _) = "tla:" ++ s

documentation = [ "tla:<revision> - A target of this form retrieves the a TLA archive with the"
                , "given revision name." ]

instance BuildTarget Tla where
    getTop _ (Tla _ tree) = topdir tree
    cleanTarget _ (Tla _ _) path =
        timeTaskAndTest (cleanStyle path (commandTask cmd))
        where
          cmd = "find '" ++ path ++ "' -name '.arch-ids' -o -name '{arch}' -prune | xargs rm -rf"
          cleanStyle path = setStart (Just ("Clean TLA target in " ++ path))

    revision _ (Tla _ tree) =
        do let path = topdir tree
               cmd = "cd " ++ path ++ " && tla revisions -f -r | head -1"
           -- FIXME: this command can take a lot of time, message it
           (_, outh, _, handle) <- liftIO $ runInteractiveCommand cmd
           revision <- (hSetBinaryMode outh True >> hGetContents outh >>= return . listToMaybe . lines) >>=
                       return . maybe (error "no revision info printed by '" ++ cmd ++ "'") id
           waitForProcess handle
           return $ "tla:" ++ revision

    logText (Tla _ _) revision = "TLA revision: " ++ maybe "none" id revision

prepareTla :: (RunClass p) => p -> String -> IO Tgt
prepareTla params version =
    do
      when (P.flushSource params) (liftIO (removeRecursiveSafely dir))
      exists <- liftIO $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      return . Tgt $ Tla version tree
    where
      verifySource dir =
          do result <- try (runTaskAndTest (verifyStyle (commandTask ("cd " ++ dir ++ " && tla changes"))))
             case result of
               Left (e :: SomeException) -> vPutStrBl 0 (show e) >> removeSource dir >> createSource dir -- Failure means there is corruption
               Right _output -> updateSource dir						         -- Success means no changes

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && tla update " ++ version))) >>
             -- At one point we did a tla undo here.  However, we are
             -- going to assume that the "clean" copies in the cache
             -- directory are clean, since some of the other target
             -- types have no way of doing this reversion.
          findSourceTree dir

      createSource dir =
          do
            -- Create parent dir and let tla create dir
            let (parent, _) = splitFileName dir
            liftIO $ createDirectoryIfMissing True parent
            runTaskAndTest (createStyle (commandTask ("tla get " ++ version ++ " " ++ dir)))
            findSourceTree dir

      verifyStyle = (setStart (Just ("Verifying TLA source archive " ++ version)) .
                     setError (Just (\ _ -> "tla changes failed in" ++ dir)) {- . Output Indented-})
      updateStyle = (setStart (Just ("Updating TLA source for " ++ version)) .
                     setError (Just (\ _ -> "updateSource failed")) {- . Output Indented -})
      createStyle = (setStart (Just ("Retrieving TLA source for " ++ version)) .
                     setError (Just (\ _ -> "tla get failed in " ++ dir)))
      dir = P.topDir params ++ "/tla/" ++ version
