{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Debian.Repo.IO
    ( AptIOT
    -- * AptIO Monad
    , io
    , tio		
    , runAptIO
    , tryAB
    -- * State
    , AptState
    , setRepoMap
    , getRepoMap
    , lookupRepository
    , insertRepository
    , lookupAptImage
    , insertAptImage
    , lookupSourcePackages
    , insertSourcePackages
    , lookupBinaryPackages
    , insertBinaryPackages
    , readParagraphs
    , findRelease
    , putRelease
    , countTasks
    ) where

import qualified Debian.Control.ByteString as B
import Debian.Repo.Types

import		 Control.Exception
import		 Control.Monad.State
import		 Control.Monad.Trans
import		 Data.Char
import		 Data.List
import qualified Data.Map as Map
import		 Data.Maybe
import		 Debian.URI
import		 Extra.TIO
import qualified System.IO as IO
import		 System.Posix.Files
import		 Text.Printf

instance Ord FileStatus where
    compare a b = compare (deviceID a, fileID a, modificationTime a) (deviceID b, fileID b, modificationTime b)

instance Eq FileStatus where
    a == b = compare a b == EQ

-- | A new monad to support the IO requirements of the autobuilder.
-- This uses the RWS monad.  The reader monad is used to store a flag
-- indicating whether this is a dry run, and the style information
-- associated with each output handle, including indentation, prefixing,
-- and replacing the output with one dot per n output characters.
-- The state monad stores information used to implement the current
-- output style and includes state information about whether the console
-- is at the beginning of a line, per-handle state information, and a
-- cache of the repositories that have been verified.
type AptIOT = StateT AptState
type AptIO = AptIOT TIO

-- | This represents the state of the IO system.  The 'bol' flag keeps
-- track of whether we are at the beginning of line on the console.
-- This is computed in terms of what we have sent to the console, but
-- it should be remembered that the order that stdout and stderr are
-- sent to the console may not be the same as the order in which they
-- show up there.  However, this appears to server our purposes for
-- now.
data AptState
    = AptState
      { repoMap :: Map.Map URI (Maybe Repository)		-- ^ Map to look up known Repository objects
      , releaseMap :: Map.Map (URI, ReleaseName) (Maybe Release) -- ^ Map to look up known Release objects
      , aptImageMap :: Map.Map SliceName (Maybe AptImage)	-- ^ Map to look up prepared AptImage objects
      , sourcePackageMap :: Map.Map FilePath (Maybe (FileStatus, [SourcePackage]))
      , binaryPackageMap :: Map.Map FilePath (Maybe (FileStatus, [BinaryPackage]))
      }

-- |mark an action that should be run in the regular IO monad
io :: CIO m => IO a -> AptIOT m a
io = lift . liftIO

-- |mark an action that should be run in the terminal IO monad
tio :: CIO m => m a -> AptIOT m a
tio = lift

-- |Perform an AptIO monad task in the IO monad.
runAptIO :: CIO m => AptIOT m a -> m a
runAptIO action = (runStateT action) initState >>= \ (a, _) -> return a

-- |Implementation of try for the AptIO monad.  If the task throws
-- an exception the initial state will be restored.
tryAB :: AptIO a -> AptIO (Either Exception a)
tryAB task =
    do
      state <- get
      liftAB (try' state) task
    where
      try' state task =
          do result <- tryTIO task
             case result of
               Left e -> return (Left e, state)
               Right (a, s) -> return (Right a, s)

      liftAB :: (TIO (a, AptState) -> TIO (b, AptState)) -> (AptIO a -> AptIO b)
      liftAB f = {- AptIO . -} mapStateT f

-- |The initial output state - at the beginning of the line, no special handle
-- state information, no repositories in the repository map.
initState :: AptState
initState = AptState
            { repoMap = Map.empty
            , releaseMap = Map.empty
            , aptImageMap = Map.empty
            , sourcePackageMap = Map.empty
            , binaryPackageMap = Map.empty
            }

setRepoMap :: Map.Map URI (Maybe Repository) -> AptState -> AptState
setRepoMap m state = state {repoMap = m}

getRepoMap :: AptState -> Map.Map URI (Maybe Repository)
getRepoMap state = repoMap state

lookupRepository :: URI -> AptState -> Maybe Repository
lookupRepository uri state = Map.findWithDefault Nothing uri (repoMap state)

insertRepository :: URI -> Repository -> AptState -> AptState
insertRepository uri repo state = state {repoMap = Map.insert uri (Just repo) (repoMap state)}

lookupAptImage :: SliceName -> AptState -> Maybe AptImage
lookupAptImage name state = Map.findWithDefault Nothing  name (aptImageMap state)

insertAptImage :: SliceName -> AptImage -> AptState -> AptState
insertAptImage name image state = state {aptImageMap = Map.insert name (Just image) (aptImageMap state)}

lookupSourcePackages :: FilePath -> AptState -> Maybe (FileStatus, [SourcePackage])
lookupSourcePackages key state = Map.findWithDefault Nothing key (sourcePackageMap state)

insertSourcePackages :: FilePath -> (FileStatus, [SourcePackage]) -> AptState -> AptState
insertSourcePackages key packages state = state {sourcePackageMap = Map.insert key (Just packages) (sourcePackageMap state)}

lookupBinaryPackages :: FilePath -> AptState -> Maybe (FileStatus, [BinaryPackage])
lookupBinaryPackages key state = Map.findWithDefault Nothing key (binaryPackageMap state)

insertBinaryPackages :: FilePath -> (FileStatus, [BinaryPackage]) -> AptState -> AptState
insertBinaryPackages key packages state =
    state {binaryPackageMap = Map.insert key (Just packages) (binaryPackageMap state)}

readParagraphs :: FilePath -> IO [B.Paragraph]
readParagraphs path =
    do --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path)			-- Debugging output
       h <- IO.openBinaryFile path IO.ReadMode
       B.Control paragraphs <- B.parseControlFromHandle path h >>= return . (either (error . show) id)
       IO.hClose h
       --IO.hPutStrLn IO.stderr ("OSImage.paragraphsFromFile " ++ path ++ " done.")	-- Debugging output
       return paragraphs

findRelease :: Repository -> ReleaseName -> AptState -> Maybe Release
findRelease repo dist state =
    Map.findWithDefault Nothing (repoURI repo, dist) (releaseMap state)

putRelease :: Repository -> ReleaseName -> Release -> AptState -> AptState
putRelease repo dist release state =
    state {releaseMap = Map.insert (repoURI repo, dist) (Just release) (releaseMap state)}

-- | Perform a list of tasks with log messages.
countTasks :: CIO m => [(String, m a)] -> m [a]
countTasks tasks =
    mapM (countTask (length tasks)) (zip [1..] tasks)
    where
      countTask :: CIO m => Int -> (Int, (String, m a)) -> m a
      countTask count (index, (message, task)) =
          (ePutStrBl (printf "[%2d of %2d] %s:" index count message)) >>
          {- setStyle (addPrefix "  ") -} task
