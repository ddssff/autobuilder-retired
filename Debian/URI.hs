module Debian.URI
    ( module Network.URI
    , URIString
    , uriToString'
    , fileFromURI
    , dirFromURI
    ) where

import Control.Exception (Exception(..), try)
--import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as L
import Extra.Net (webServerDirectoryContents)
import Network.URI
import System.Directory (getDirectoryContents)
import System.Exit
import System.Unix.Process (lazyCommand, collectOutput)

uriToString' uri = uriToString id uri ""

instance Ord URI where
    compare a b = compare (uriToString' a) (uriToString' b)

-- |If the URI type could be read and showed this wouldn't be necessary.
type URIString = String

fileFromURI :: URI -> IO (Either Exception L.ByteString)
fileFromURI uri =
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> try (L.readFile (uriPath uri))
      ("ssh:", Just auth) -> cmdOutput ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++
                                        " cat " ++ show (uriPath uri))
      _ -> cmdOutput ("curl -s -g '" ++ uriToString' uri ++ "'")

dirFromURI :: URI -> IO (Either Exception [String])
dirFromURI uri =
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> try (getDirectoryContents (uriPath uri))
      ("ssh:", Just auth) -> cmdOutput ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++
                                        " ls -1 " ++ uriPath uri) >>=
                             return . either Left (Right . lines . L.unpack)
      _ -> cmdOutput ("curl -s -g '" ++ uriToString' uri ++ "/'") >>= return . either Left (Right . webServerDirectoryContents)

cmdOutput :: String -> IO (Either Exception L.ByteString)
cmdOutput cmd =
    do (out, _err, code) <- lazyCommand cmd L.empty >>= return . collectOutput
       case code of
         (ExitSuccess : _) -> return (Right out)
         (ExitFailure _ : _) -> return . Left . ErrorCall $ "Failure: " ++ show cmd
         [] -> return . Left . ErrorCall $ "Failure: no exit code"
