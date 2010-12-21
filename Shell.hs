{-# LANGUAGE PackageImports #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
-- |Control the progress reporting and output of subprocesses.
-- This module probably belongs in haskell-unixutils.
module Shell
    ( V
    , Flag(..)
    , runV
    , lazyCommandV
    , prefixes
    , printOutput
    , tests
    ) where

import Control.Exception ( evaluate )
import Control.Monad (when)
import Control.Monad.State (StateT, get, put, evalStateT)
import "mtl" Control.Monad.Trans ( MonadIO, liftIO, lift )
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
    -- ( empty, pack, unpack, span, tail )
import Data.List ( intercalate )
import Data.Time ( NominalDiffTime, getCurrentTime, diffUTCTime )
import CIO
    ( dotOutput,
      vMessage,
      ePutStrBl,
      vEPutStrBl,
      ePutStr,
      eBOL,
      ev,
      setStyle,
      addPrefixes )
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr, hPutStr)
import System.Unix.Process
    ( lazyProcess, lazyCommand, Output(Stdout, Stderr), checkResult, collectOutputUnpacked )
import System.Unix.ProcessExtra (echoCommand, echoProcess)
import qualified Data.Set as Set

type V a = StateT (Set.Set Flag) IO a
data Flag = Echo | Dots | All deriving (Ord, Eq)
runV :: [Flag] -> V a -> IO a
runV flags action = evalStateT action (Set.fromList flags)

-- |Run lazyCommand in the V monad.
lazyCommandV :: String -> L.ByteString -> V [Output]
lazyCommandV cmd input =
    get >>= \ s -> 
    when (Set.member Echo s) (lift (hPutStrLn stderr ("+" ++ cmd))) >>
    lift (lazyCommand cmd input) >>= \ output ->
    case () of
      _ | Set.member All s -> lift (printOutput (prefixes opre epre output))
        | Set.member Dots s -> lift (dotOutput 128 output)
        | True -> return output
    where
      opre = L.pack "[1] "
      epre = L.pack "[2] "

-- |Add prefixes to the output stream
prefixes :: L.ByteString -> L.ByteString -> [Output] -> [Output]
prefixes opre epre output =
    f True output
    where
      f :: Bool -> [Output] -> [Output]
      f _ [] = []
      f bol (Stdout s : output') = doString bol (map Stdout . L.toChunks) opre s output'
      f bol (Stderr s : output') = doString bol (map Stderr . L.toChunks) epre s output'
      doString bol cstr pre s output' =
          let (a, b) = L.span (/= '\n') (L.fromChunks [s]) in
          if L.null a
          then if L.null b
               then f bol output'
               else cstr (L.pack "\n") ++ f True (cstr (L.tail b) ++ output')
          else (if bol then (cstr pre ++ cstr a) else (cstr a)) ++ f False (cstr b ++ output')

-- |Print all the output to the appropriate output channel
printOutput :: [Output] -> IO [Output]
printOutput output =
    mapM print output
    where
      print x@(Stdout s) = putStr (L.unpack (L.fromChunks [s])) >> return x
      print x@(Stderr s) = hPutStr stderr (L.unpack (L.fromChunks [s])) >> return x
      print x = return x

vOutput :: Int -> [Output] -> IO [Output]
vOutput v output =
    do v' <- ev v
       --lift (IO.hPutStr IO.stderr ("(ev=" ++ show (verbosity style) ++ "-" ++ show v ++ "=" ++ show ev ++ ")"))
       case () of
         _ | v' <= (-2) -> return output
           | v' <= (-1) -> dotOutput 128 output
           | True -> setStyle (addPrefixes "[1] " "[2] ") (printOutput output)

-- |Perform a task, print the elapsed time it took, and return the result.
showElapsed :: String -> IO a -> IO a
showElapsed label f =
    do (result, time) <- timeTask f
       ePutStr (label ++ formatTime' time)
       return result

formatTime' :: NominalDiffTime -> String
formatTime' diff = show diff
{-
    case () of
      _ | isPrefixOf "00:00:0" hms -> drop 7 hms ++ printf ".%03d" ms ++ " s."
      _ | isPrefixOf "00:00:" hms -> drop 6 hms ++ printf ".%03d" ms ++ " s."
      _ | isPrefixOf "00:" hms -> drop 3 hms
      _ -> hms
    where
      hms = formatTime defaultTimeLocale "%T" diff
      (s, ms) = second toMilliseconds (properFraction diff) :: (Integer, Integer)
      toMilliseconds :: (RealFrac a, Integral b) => a -> b
      toMilliseconds f = round (f * 1000)
-}

-- |Run a command and return its result along with the amount of time it took.
timeCommand :: IO [Output] -> IO ([Output], NominalDiffTime)
timeCommand result = timeTask result

-- |Not sure if this is a useful approach.
class ShellTask a where
    command :: a -> Either String (FilePath, [String], Maybe FilePath, Maybe [(String, String)])
    input :: a -> B.ByteString					-- The command input
    input _ = B.empty
    quietness :: a -> Int					-- Verbosity level required for full output
    quietness _ = 0
    introMsg :: a -> Maybe String				-- Message printed before command starts
    introMsg _ = Nothing
    -- If failMsg or finishMsg are given, the command output will be forced
    failMsg :: a -> Maybe (Int -> String)			-- Message printed on failure
    failMsg _ = Nothing
    finishMsg :: a -> Maybe String				-- Message printed on successful finish
    finishMsg _ = Nothing

data SimpleTask = SimpleTask Int String

instance ShellTask SimpleTask where
    command (SimpleTask _ s) = (Left s)
    quietness (SimpleTask n _) = n
    introMsg (SimpleTask _ _) = Nothing

data FullTask =
    FullTask { taskQuietness :: Int
             , taskCommand :: Either String (FilePath, [String], Maybe FilePath, Maybe [(String, String)])
             , taskIntroMsg :: Maybe String
             , taskFailMsg :: Maybe (Int -> String)
             , taskFinishMsg :: Maybe String }

commandTask :: String -> FullTask
commandTask command =
    FullTask { taskQuietness = 0
             , taskCommand = Left command
             , taskIntroMsg = Nothing
             , taskFailMsg = Nothing
             , taskFinishMsg = Nothing }

processTask :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> FullTask
processTask exec args path env =
    FullTask { taskQuietness = 0
             , taskCommand = Right (exec, args, path, env)
             , taskIntroMsg = Nothing
             , taskFailMsg = Nothing
             , taskFinishMsg = Nothing }

setStart :: (Maybe String) -> FullTask -> FullTask
setStart s task = task {taskIntroMsg = s}

setFinish :: (Maybe String) -> FullTask -> FullTask
setFinish s task = task {taskFinishMsg = s}

setError :: (Maybe (Int -> String)) -> FullTask -> FullTask
setError s task = task {taskFailMsg = s}

showCommand :: ShellTask a => a -> [Char]
showCommand task = either id (\ (exec, args, _, _) -> intercalate " " ([exec] ++ args)) (command task)

instance ShellTask FullTask where
    quietness = taskQuietness
    command = taskCommand
    introMsg = taskIntroMsg
    failMsg = taskFailMsg
    finishMsg = taskFinishMsg

runTask :: (ShellTask a) => a -> IO [Output]
runTask task =
    liftIO (either (\ cmd -> lazyCommand cmd (L.fromChunks [input task]))
                   (\ (exec, args, path, env) -> lazyProcess exec args path env (L.fromChunks [input task])) (command task)) >>=
    maybe return (vMessage 0) (introMsg task) >>=
    vOutput 2 >>=
    (\ output ->
         (case (failMsg task, finishMsg task) of
            (Nothing, Nothing) -> return output
            _ -> checkResult (onFail task) (onFinish task) output >> return output))
    where
      onFail :: (ShellTask a) => a -> Int -> IO ()
      onFail task n = maybe (return ()) (\ f -> vEPutStrBl 0 (f n)) (failMsg task)
      onFinish :: (ShellTask a) => a -> IO ()
      onFinish task = maybe (return ()) (\ s -> vEPutStrBl 0 s) (finishMsg task)

runTaskAndTest :: (ShellTask a) => a -> IO [Output]
runTaskAndTest task =
    do output <- runTask task
       checkResult failure (ok output) output
    where
      failure n = fail (maybe ("*** FAILURE: " ++ showCommand task ++ " -> " ++ show n) (\ f -> f n) (failMsg task))
      ok output = return output

-- |Run a task and return the elapsed time along with its result.
timeTask :: MonadIO m => m a -> m (a, NominalDiffTime)
timeTask x =
    do start <- liftIO getCurrentTime
       result <- x >>= liftIO . evaluate
       finish <- liftIO getCurrentTime
       return (result, diffUTCTime finish start)

timeTaskAndTest :: (ShellTask a) => a -> IO ([Output], NominalDiffTime)
timeTaskAndTest task = timeTask (runTaskAndTest task)

-- Reimplementations of old functions

runCommand :: Int -> String -> IO [Output]
runCommand v cmd = runTaskAndTest (SimpleTask v cmd)

runCommandTimed :: Int -> String -> IO ([Output], NominalDiffTime)
runCommandTimed v cmd = timeCommand (runCommand v cmd)

runCommandQuietly :: String -> IO [Output]
runCommandQuietly = runCommand 1

runCommandQuietlyTimed :: String -> IO ([Output], NominalDiffTime)
runCommandQuietlyTimed = runCommandTimed 1

runCommandMsg :: Int -> Maybe String -> String -> (Int -> IO (Either String ())) -> IO (Either String ())
runCommandMsg v start cmd fail =
    -- If the program is run with -v -v, v0 will be 2, meaning quit verbose output.
    -- The higher the v argument is the higher the v0 argument must be to achieve
    -- the same verbosity, so the "effective verbosity" ev is v0 - v.
    do eBOL >> 
            maybe (return ()) (vEPutStrBl v) start >>
            vEPutStrBl (v+1) ("# " ++ cmd) >>
            liftIO (lazyCommand cmd L.empty) >>=
            vOutput (v+2) >>=
            checkResult fail (return (Right ()))

system' :: String -> IO ()
system' cmd =
    hPutStrLn stderr ("> " ++ cmd) >>
    lazyCommand cmd L.empty >>= return . collectOutputUnpacked >>= \ (out, err, codes) ->
    case codes of
      [ExitSuccess] -> return ()
      _ -> fail $ cmd ++ " -> " ++ show codes ++ "\n stdout:\n\n" ++ show out ++ "\n\nstderr:\n\n" ++ show err

tests :: [[Output]]
tests =
    [prefixes (L.pack "[1] ") (L.pack "[2] ") [Stdout (B.pack "abc\ndef\n")]
    ]