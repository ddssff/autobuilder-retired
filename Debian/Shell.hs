-- |This module probably belongs in haskell-unixutils.
module Debian.Shell 
    ( echoCommand
    , echoProcess
    , dotOutput
    , vOutput
    -- * Semi-obsolete
    , runCommand
    , runCommandQuietly
    , runCommandTimed
    , runCommandQuietlyTimed
    , runCommandMsg
    -- * Type Class
    , ShellTask(..)
    , SimpleTask(..)
    , FullTask(..)
    , commandTask
    , processTask
    , showCommand
    , setStart
    , setFinish
    , setError
    , runTask
    , runTaskAndTest
    , timeTask
    , timeTaskAndTest
    --, runCommandDots
    , timeCommand
    , showElapsed
    ) where

import		 Control.Exception
import		 Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy.Char8 as L
import		 Data.List
import           Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime {-, formatTime, timeToTimeOfDay, secondsToDiffTime-})
import		 Debian.Extra.CIO (dotOutput, printOutput, vMessage)
import		 Extra.CIO (CIO, ePutStrBl, vEPutStrBl, ePutStr, eBOL, ev, setStyle, addPrefixes)
import		 Prelude hiding (putStr)
import		 System.Unix.Process -- as P

echoCommand :: CIO m => String -> L.ByteString -> m [Output]
echoCommand command input =
    ePutStrBl ("# " ++ command) >>
    liftIO (lazyCommand command input)

-- |Echo the process arguments and then run the process
echoProcess :: CIO m => FilePath -> [String] -> L.ByteString -> m [Output]
echoProcess exec args input =
    ePutStrBl (intercalate " " ("#" : exec : args)) >>
    liftIO (lazyProcess exec args Nothing Nothing input)

vOutput :: CIO m => Int -> [Output] -> m [Output]
vOutput v output =
    do v' <- ev v
       --lift (IO.hPutStr IO.stderr ("(ev=" ++ show (verbosity style) ++ "-" ++ show v ++ "=" ++ show ev ++ ")"))
       case () of
         _ | v' <= (-2) -> return output
           | v' <= (-1) -> dotOutput 128 output
           | True -> setStyle (addPrefixes "[1] " "[2] ") (printOutput output)

-- |Perform a task, print the elapsed time it took, and return the result.
showElapsed :: CIO m => String -> m a -> m a
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
timeCommand :: CIO m => m (Either String [Output]) -> m (Either String ([Output], NominalDiffTime))
timeCommand result =
    timeTask result >>= \ (result, elapsed) -> return (either Left (\ output -> Right (output, elapsed)) result)

-- |Not sure if this is a useful approach.
class ShellTask a where
    command :: a -> Either String (FilePath, [String], Maybe FilePath, Maybe [(String, String)])
    input :: a -> L.ByteString					-- The command input
    input _ = L.empty
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

commandTask command =
    FullTask { taskQuietness = 0
             , taskCommand = Left command
             , taskIntroMsg = Nothing
             , taskFailMsg = Nothing
             , taskFinishMsg = Nothing }

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

showCommand task = either id (\ (exec, args, _, _) -> intercalate " " ([exec] ++ args)) (command task)

instance ShellTask FullTask where
    quietness = taskQuietness
    command = taskCommand
    introMsg = taskIntroMsg
    failMsg = taskFailMsg
    finishMsg = taskFinishMsg

runTask :: (ShellTask a, CIO m) => a -> m [Output]
runTask task =
    liftIO (either (\ cmd -> lazyCommand cmd (input task)) 
                       (\ (exec, args, path, env) -> lazyProcess exec args path env (input task)) (command task)) >>=
    maybe return (vMessage 0) (introMsg task) >>=
    vOutput 2 >>=
    (\ output ->
         (case (failMsg task, finishMsg task) of
            (Nothing, Nothing) -> return output
            _ -> checkResult (onFail task) (onFinish task) output >> return output))
    where
      onFail :: (ShellTask a, CIO m) => a -> Int -> m ()
      onFail task n = maybe (return ()) (\ f -> vEPutStrBl 0 (f n)) (failMsg task)
      onFinish :: (ShellTask a, CIO m) => a -> m ()
      onFinish task = maybe (return ()) (\ s -> vEPutStrBl 0 s) (finishMsg task)

runTaskAndTest :: (ShellTask a, CIO m) => a -> m (Either String [Output])
runTaskAndTest task =
    do output <- runTask task
       checkResult fail (ok output) output
    where
      fail n = return (Left (maybe ("*** FAILURE: " ++ showCommand task ++ " -> " ++ show n) (\ f -> f n) (failMsg task)))
      ok output = return (Right output)

-- |Run a task and return the elapsed time along with its result.
timeTask :: MonadIO m => m a -> m (a, NominalDiffTime)
timeTask x =
    do start <- liftIO getCurrentTime
       result <- x >>= liftIO . evaluate
       finish <- liftIO getCurrentTime
       return (result, diffUTCTime finish start)

timeTaskAndTest :: (ShellTask a, CIO m) => a -> m (Either String ([Output], NominalDiffTime))
timeTaskAndTest task =
    timeTask (runTaskAndTest task) >>= return . fixResult
    where
      fixResult (Left x, _) = Left x
      fixResult (Right x, t) = Right (x, t)

-- Reimplementations of old functions

runCommand :: CIO m => Int -> String -> m (Either String [Output])
runCommand v cmd = runTaskAndTest (SimpleTask v cmd)

runCommandTimed :: CIO m => Int -> String -> m (Either String ([Output], NominalDiffTime))
runCommandTimed v cmd = timeCommand (runCommand v cmd)

runCommandQuietly :: CIO m => String -> m (Either String [Output])
runCommandQuietly = runCommand 1

runCommandQuietlyTimed :: CIO m => String -> m (Either String ([Output], NominalDiffTime))
runCommandQuietlyTimed = runCommandTimed 1

runCommandMsg :: CIO m => Int -> Maybe String -> String -> (Int -> m (Either String ())) -> m (Either String ())
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
