module Debian.Version.ByteString
    ( ParseDebianVersion(..)
    ) where

import Text.ParserCombinators.Parsec

import qualified Data.ByteString.Char8 as C

import Debian.Version.Common
import Debian.Version.Internal
    
instance ParseDebianVersion C.ByteString where
    parseDebianVersion byteStr =
        let str = C.unpack byteStr in
        case parse parseDV str str of
          Left e -> error (show e)
          Right dv -> DebianVersion str dv
