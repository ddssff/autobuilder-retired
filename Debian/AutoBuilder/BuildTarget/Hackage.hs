{-# LANGUAGE ScopedTypeVariables #-}
-- |A 'hackage:' target of the form hackage:<name> or hackage:<name>=<version> pulls
-- source from hackage.haskell.org.
module Debian.AutoBuilder.BuildTarget.Hackage where

import qualified Data.ByteString.Lazy as B
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes)
import Debian.AutoBuilder.BuildTarget
import Debian.AutoBuilder.ParamClass (RunClass)
import Debian.Repo
import System.Exit
import System.Unix.Process (collectOutputUnpacked)
import System.Unix.Progress (lazyCommandF)
import Text.XML.HaXml (htmlprint)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Html.Parse (htmlParse)
import Text.XML.HaXml.Posn
import Text.Regex

data Hackage = Hackage String (Maybe String) SourceTree

instance Show Hackage where
    show (Hackage name version _) = "hackage:" ++ name ++ maybe "" ("=" ++) version

documentation = [ "hackage:<name> or hackage:<name>=<version> - a target of this form"
                , "retrieves source code from http://hackage.haskell.org." ]

instance BuildTarget Hackage where
    getTop _ (Hackage _ _ tree) = topdir tree
    revision _ (Hackage name (Just version) _) = return $ "hackage:" ++ name ++ "=" ++ version
    revision _ (Hackage _ Nothing _) = fail "Attempt to generate revision string for unversioned hackage target"
    logText (Hackage name _ _) revision = "Built from hackage, revision: " ++ either show id revision

prepareHackage :: (RunClass p) => p -> String -> IO Tgt
prepareHackage params target =
    getVersion (name, version) >>= \ version' ->
    download name version' >>=
    unpack
    where
      (name, version) = parseTarget target
      -- Extract the version number from the package page.
      getVersion (name, (Just version)) = return version
      getVersion (name, Nothing) =
          lazyCommandF cmd B.empty >>= return . findVersion name . parse cmd
          where cmd = curlCmd (packageURL name)
      download = unimplemeneted
      unpack = unimplemeneted

parse cmd output =
    case collectOutputUnpacked output of
      (out, _, ExitSuccess) -> htmlParse cmd out
      (_, _, _) -> error (cmd ++ " -> " ++ show output)

curlCmd url = "curl -s '" ++ url ++ "'"

parseTarget target =
    case matchRegex (mkRegex "^([^=]+)(=(.*))?$") target of
      Just [s, _, ""] -> (s, Nothing)
      Just [s, _, v] -> (s, Just v)
      _ -> error $ "Invalid hackage target: " ++ show target

packageURL name = "http://hackage.haskell.org/package/" ++ name

findVersion :: String -> Document Posn -> String
findVersion package (Document _ _ (Elem name attrs content) _) =
    case doContentList content of
      [s] -> s
      ss -> error ("findVersion: " ++ show ss)
    where
      doContentList [CElem (Elem "head" _ _) _, CElem (Elem "body" _ content) _] = doContentList content
      doContentList [CElem (Elem "div" _ _) _, CElem (Elem "div" _ content) _, CElem (Elem "div" _ _) _] = doContentList content
      doContentList [_, _, _, _, _, _, _, CElem (Elem "ul" _ content) _] = doContentList content
      doContentList [CElem (Elem "li" _ content) _, _] = doContentList content
      doContentList [CElem (Elem "a" _ content) _, _] = doContentList content
      doContentList xs = concatMap doContent xs -- [show (map (htmlprint . (: [])) xs)]
      doContent (CElem (Elem name attrs content) i) = ["CElem " ++ name]
      doContent (CString b c i) = [parseTarball c]
      doContent (CRef r i) = []
      doContent (CMisc m i) = []
      parseTarball s =
          let prefix = package ++ "-"
              suffix = ".tar.gz" in
          if isPrefixOf prefix s && isSuffixOf suffix s
          then let s' = drop (length prefix) s in
               take (length s' - length suffix) s'
          else error $ "findVersion - not a tarball: " ++ show s

unimplemeneted = undefined
