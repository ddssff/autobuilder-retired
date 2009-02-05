module Debian.Time where

import Data.Time
import Data.Time.Clock.POSIX
import System.Locale (defaultTimeLocale)
import System.Posix.Types

-- * Time Helper Functions

rfc822DateFormat :: String
rfc822DateFormat = "%a, %d %b %Y %T %z"

epochTimeToUTCTime :: EpochTime -> UTCTime
epochTimeToUTCTime = posixSecondsToUTCTime . fromIntegral . fromEnum

formatTimeRFC822 :: (FormatTime t) => t -> String
formatTimeRFC822 = formatTime defaultTimeLocale rfc822DateFormat

parseTimeRFC822 :: (ParseTime t) => String -> Maybe t
parseTimeRFC822 = parseTime defaultTimeLocale rfc822DateFormat

getCurrentLocalRFC822Time :: IO String
getCurrentLocalRFC822Time = getCurrentTime >>= utcToLocalZonedTime >>= return . formatTime defaultTimeLocale rfc822DateFormat

