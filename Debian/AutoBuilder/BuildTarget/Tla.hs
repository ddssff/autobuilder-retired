{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Tla where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import Debian.AutoBuilder.BuildTarget.Common
import qualified Debian.AutoBuilder.BuildTarget.Temp as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Repo
import System.FilePath (splitFileName)
import System.IO
import System.Process
import System.Unix.Directory
import System.Unix.Progress (timeTask, lazyCommandF)
import System.Unix.QIO (qPutStrLn)
import System.Directory

-- | A TLA archive
data Tla = Tla String SourceTree R.RetrieveMethod

documentation = [ "tla:<revision> - A target of this form retrieves the a TLA archive with the"
                , "given revision name." ]

instance Download Tla where
    method (Tla _ _ m) = m
    getTop _ (Tla _ tree _) = topdir tree
    revision _ (Tla _ tree _) =
        do let path = topdir tree
               cmd = "cd " ++ path ++ " && tla revisions -f -r | head -1"
           -- FIXME: this command can take a lot of time, message it
           (_, outh, _, handle) <- liftIO $ runInteractiveCommand cmd
           revision <- (hSetBinaryMode outh True >> hGetContents outh >>= return . listToMaybe . lines) >>=
                       return . maybe (error "no revision info printed by '" ++ cmd ++ "'") id
           _output <- waitForProcess handle
           return $ "tla:" ++ revision
    logText (Tla _ _ _) revision = "TLA revision: " ++ either show id revision
    cleanTarget _ (Tla _ _ _) path =
        -- timeTaskAndTest (cleanStyle path (commandTask cmd))
        timeTask (lazyCommandF cmd L.empty)
        where
          cmd = "find '" ++ path ++ "' -name '.arch-ids' -o -name '{arch}' -prune | xargs rm -rf"
          -- cleanStyle path = setStart (Just ("Clean TLA target in " ++ path))

prepare :: P.CacheRec -> String -> R.RetrieveMethod -> AptIOT IO T.Download
prepare cache version m = liftIO $
    do
      when (P.flushSource (P.params cache)) (liftIO (removeRecursiveSafely dir))
      exists <- liftIO $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      -- return $ Tla version tree m
      rev <- do let path = topdir tree
                    cmd = "cd " ++ path ++ " && tla revisions -f -r | head -1"
                -- FIXME: this command can take a lot of time, message it
                (_, outh, _, handle) <- liftIO $ runInteractiveCommand cmd
                revision <- (hSetBinaryMode outh True >> hGetContents outh >>= return . listToMaybe . lines) >>=
                            return . maybe (error "no revision info printed by '" ++ cmd ++ "'") id
                _output <- waitForProcess handle
                return $ "tla:" ++ revision
      return $ T.Download { T.method' = m
                          , T.getTop = topdir tree
                          , T.revision = rev
                          , T.logText =  "TLA revision: " ++ rev
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ path -> 
                                  let cmd = "find '" ++ path ++ "' -name '.arch-ids' -o -name '{arch}' -prune | xargs rm -rf" in
                                  timeTask (lazyCommandF cmd L.empty)
                          }
    where
      verifySource dir =
          do -- result <- try (runTaskAndTest (verifyStyle (commandTask ("cd " ++ dir ++ " && tla changes"))))
             result <- try (lazyCommandF ("cd " ++ dir ++ " && tla changes") L.empty)
             case result of
               Left (e :: SomeException) -> qPutStrLn (show e) >> removeSource dir >> createSource dir -- Failure means there is corruption
               Right _output -> updateSource dir						         -- Success means no changes

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          -- runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && tla update " ++ version))) >>
          lazyCommandF ("cd " ++ dir ++ " && tla update " ++ version) L.empty >>
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
            -- runTaskAndTest (createStyle (commandTask ("tla get " ++ version ++ " " ++ dir)))
            _output <- lazyCommandF ("tla get " ++ version ++ " " ++ dir) L.empty
            findSourceTree dir

{-
      verifyStyle = (setStart (Just ("Verifying TLA source archive " ++ version)) .
                     setError (Just (\ _ -> "tla changes failed in" ++ dir)) {- . Output Indented-})
      updateStyle = (setStart (Just ("Updating TLA source for " ++ version)) .
                     setError (Just (\ _ -> "updateSource failed")) {- . Output Indented -})
      createStyle = (setStart (Just ("Retrieving TLA source for " ++ version)) .
                     setError (Just (\ _ -> "tla get failed in " ++ dir)))
-}
      dir = P.topDir cache ++ "/tla/" ++ version
