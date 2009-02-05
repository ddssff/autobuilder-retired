module Debian.Deb where

import Control.Monad

import Debian.Control.Common
import System.Unix.Directory
import System.Unix.FilePath
import System.Unix.Process

fields :: (ControlFunctions a) => FilePath -> IO (Control' a)
fields debFP =
    withTemporaryDirectory ("fields.XXXXXX") $ \tmpdir ->
      do debFP <- realpath debFP
         withWorkingDirectory tmpdir $ 
           do (out, err, res) <- simpleProcess "ar" ["x",debFP,"control.tar.gz"] 
              when (res /= ExitSuccess) (error $ "Dpkg.fields: " ++ out ++ "\n" ++ err ++ "\n" ++ show res)
              (out, err, res) <- simpleProcess "tar" ["xzf", "control.tar.gz", "./control"]
              when (res /= ExitSuccess) (error $ "Dpkg.fields: " ++ out ++ "\n" ++ err ++ "\n" ++ show res)
              c <- parseControlFromFile "control" 
              case c of
                Left e -> error (show e)
                (Right c) -> return c -- I don't think we need seq because parsec will force everything from the file
