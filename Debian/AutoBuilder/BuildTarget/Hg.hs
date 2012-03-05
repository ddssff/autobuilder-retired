{-# LANGUAGE ScopedTypeVariables #-}
-- | A Mercurial archive.
module Debian.AutoBuilder.BuildTarget.Hg where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Lazy.Char8 (empty)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Repo
import System.Directory
import System.FilePath (splitFileName)
import System.Unix.Directory
import System.Unix.Progress (lazyCommandF, timeTask)

documentation = [ "hg:<string> - A target of this form target obtains the source"
                , "code by running the Mercurial command 'hg clone <string>'." ]

prepare :: P.CacheRec -> R.RetrieveMethod -> String -> AptIOT IO T.Download
prepare cache m archive = liftIO $
    do
      when (P.flushSource (P.params cache)) (liftIO $ removeRecursiveSafely dir)
      exists <- liftIO $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      return $ T.Download { T.method = m
                          , T.getTop = topdir tree
                          , T.logText =  "Hg revision: " ++ show m
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ path -> 
                                  let cmd = "rm -rf " ++ path ++ "/.hg" in
                                  timeTask (lazyCommandF cmd empty)
                          , T.buildWrapper = id
                          }
    where
      verifySource dir =
          try (lazyCommandF ("cd " ++ dir ++ " && hg status | grep -q .") empty) >>=
          either (\ (_ :: SomeException) -> updateSource dir)	-- failure means there were no changes
                 (\ _ -> removeSource dir >> createSource dir)	-- success means there was a change

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          lazyCommandF ("cd " ++ dir ++ " && hg pull -u") empty >>
          findSourceTree dir
            

      createSource dir =
          let (parent, _) = splitFileName dir in
          liftIO (createDirectoryIfMissing True parent) >>
          lazyCommandF ("hg clone " ++ archive ++ " " ++ dir) empty >>
          findSourceTree dir

      dir = P.topDir cache ++ "/hg/" ++ archive

