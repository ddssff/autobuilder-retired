module Debian.Extra.CIO
    ( tMessage
    , vMessage
    , printStdout
    , printStderr
    , printOutput
    , dotOutput
    ) where

import qualified Data.ByteString.Char8 as B
import Extra.CIO
import Prelude hiding (putStr)
import System.Unix.Process

-- |Print a message without forcing the command's output
tMessage :: CIO m => String -> a -> m a
tMessage message output = ePutStrBl message >> return output

-- |Print a message without forcing the command's output
vMessage :: CIO m => Int -> String -> a -> m a
vMessage v message output = vEPutStrBl v message >> return output

-- |Print stdout to stdout
printStdout :: CIO m => [Output] -> m [Output]
printStdout output =
    bol >> mapM print output
    where
      print x@(Stdout s) = putStr (B.unpack s) >> return x
      print x = return x

-- |Print stderr to stderr
printStderr :: CIO m => [Output] -> m [Output]
printStderr output =
    eBOL >> mapM print output
    where
      print x@(Stderr s) = ePutStr (B.unpack s) >> return x
      print x = return x

-- |Print all the output to the appropriate output channel
printOutput :: CIO m => [Output] -> m [Output]
printOutput output =
    eBOL >> mapM print output
    where
      print x@(Stdout s) = putStr (B.unpack s) >> return x
      print x@(Stderr s) = ePutStr (B.unpack s) >> return x
      print x = return x

-- |Print one dot to stderr for every COUNT characters of output.
dotOutput :: CIO m => Int -> [Output] -> m [Output]
dotOutput groupSize output =
    mapM (\ (count, elem) -> ePutStr (replicate count '.') >> return elem) pairs
    where
      pairs = zip (dots 0 (map length output)) output
      dots _ [] = []
      dots rem (count : more) =
          let (count', rem') = divMod (count + rem) groupSize in
          count' : dots rem' more
      length (Stdout s) = B.length s
      length (Stderr s) = B.length s
      length _ = 0

