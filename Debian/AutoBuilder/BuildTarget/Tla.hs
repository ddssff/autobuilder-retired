{-# LANGUAGE ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Tla where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import qualified Debian.AutoBuilder.Types.Download as T
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
-- data Tla = Tla String SourceTree R.RetrieveMethod

documentation = [ "tla:<revision> - A target of this form retrieves the a TLA archive with the"
                , "given revision name." ]

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
      return $ T.Download { T.method = m
                          , T.getTop = topdir tree
                          , T.revision = rev
                          , T.logText =  "TLA revision: " ++ rev
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ path -> 
                                  let cmd = "find '" ++ path ++ "' -name '.arch-ids' -o -name '{arch}' -prune | xargs rm -rf" in
                                  timeTask (lazyCommandF cmd L.empty)
                          , T.buildWrapper = id
                          }
    where
      verifySource dir =
          do result <- try (lazyCommandF ("cd " ++ dir ++ " && tla changes") L.empty)
             case result of
               Left (e :: SomeException) -> qPutStrLn (show e) >> removeSource dir >> createSource dir -- Failure means there is corruption
               Right _output -> updateSource dir						         -- Success means no changes

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
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
            _output <- lazyCommandF ("tla get " ++ version ++ " " ++ dir) L.empty
            findSourceTree dir

      dir = P.topDir cache ++ "/tla/" ++ version
