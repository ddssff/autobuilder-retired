-- | A Mercurial archive.
module BuildTarget.Hg where

import Control.Monad
import Data.Maybe
import System.Directory
import System.IO
import System.Process
import Linspire.Unix.Directory
import Linspire.Unix.FilePath
import BuildTarget
import Debian.SourceTree
import Debian.IO
import Debian.Types

data Hg = Hg String SourceTree

instance Show Hg where
    show (Hg s _) = "hg:" ++ s

documentation = [ "hg:<string> - A target of this form target obtains the source"
                , "code by running the Mercurial command 'hg clone <string>'." ]

instance BuildTarget Hg where
    getTop (Hg _ tree) = dir tree
    --getSourceTree (Hg _ tree) = tree
    --setSpecTree (Hg s _) tree = Hg s tree

    revision (Hg _ tree) =
        do let path = dir tree
               cmd = "cd " ++ outsidePath path ++ " && hg log -r $(hg id | cut -d' ' -f1 )"
           (_, outh, _, handle) <- io $ runInteractiveCommand cmd
           revision <- io (hGetContents outh) >>= return . listToMaybe . lines >>=
                       return . maybe (error "no revision info printed by '" ++ cmd ++ "'") id
           io $ waitForProcess handle
           return . Just $ "hg:" ++ revision

    cleanTarget (Hg _ _) source =
        do cleanStyle path $ systemTask_ cmd
           return ()
        where
          path = dir source
          cmd = "rm -rf " ++ outsidePath path ++ "/.hg"
          cleanStyle path = setStyle (setStart (Just ("Clean Hg target in " ++ show path)))

    logText (Hg _ _) revision = "Hg revision: " ++ maybe "none" id revision

prepareHg :: Bool -> FilePath -> Bool -> String -> AptIO Tgt
prepareHg _debug top flush archive =
    do
      when flush (io $ removeRecursiveSafely dir)
      exists <- io $ doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      case tree of
        Nothing -> error ("Failed to find HG source tree at " ++ show dir)
        Just tree -> return $ Tgt $ Hg archive tree
    where
      verifySource dir =
          tryAB (verifyStyle $ systemTask ("cd " ++ dir ++ " && hg status | grep -q .")) >>=
          either (\ _ -> updateSource dir)	-- failure means there were no changes
                 (\ _ -> removeSource dir >> createSource dir)	-- success means there was a change

      removeSource dir = io $ removeRecursiveSafely dir

      updateSource dir =
          do
            updateStyle $ systemTask ("cd " ++ dir ++ " && hg pull -u")
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
            createStyle $ systemTask ("hg clone " ++ archive ++ " " ++ dir)
            findSourceTree (rootEnvPath dir)

      verifyStyle = setStyle (setStart (Just ("Verifying Hg source archive " ++ archive)) .
                              setError (Just ("tla changes failed in" ++ show dir)))
      updateStyle = setStyle (setStart (Just ("Updating Hg source for " ++ archive)) .
                              setError (Just ("Update Hg Source failed in " ++ show dir)))
      createStyle = setStyle (setStart (Just ("Retrieving Hg source for " ++ archive)) .
                              setError (Just ("hg clone failed in " ++ show dir)) .
                              setEcho True)
      dir = top ++ "/hg/" ++ archive

