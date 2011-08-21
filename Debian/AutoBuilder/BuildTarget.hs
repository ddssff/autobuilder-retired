module Debian.AutoBuilder.BuildTarget
    ( readSpec
    , readSpec'
    , targetDocumentation
    ) where

import Control.Applicative ((<$>))
import Control.Arrow(second)
import Control.Monad.Trans (lift)
import Data.List (intersperse)
import qualified Debian.AutoBuilder.BuildTarget.Apt as Apt
import qualified Debian.AutoBuilder.BuildTarget.Cd as Cd
import qualified Debian.AutoBuilder.BuildTarget.Darcs as Darcs
import qualified Debian.AutoBuilder.BuildTarget.DebDir as DebDir
import qualified Debian.AutoBuilder.BuildTarget.Debianize as Debianize
import qualified Debian.AutoBuilder.BuildTarget.Dir as Dir
import qualified Debian.AutoBuilder.BuildTarget.Hackage as Hackage
import qualified Debian.AutoBuilder.BuildTarget.Hg as Hg
import qualified Debian.AutoBuilder.BuildTarget.Proc as Proc
import qualified Debian.AutoBuilder.BuildTarget.Quilt as Quilt
import qualified Debian.AutoBuilder.BuildTarget.SourceDeb as SourceDeb
import qualified Debian.AutoBuilder.BuildTarget.Svn as Svn
import qualified Debian.AutoBuilder.BuildTarget.Tla as Tla
import qualified Debian.AutoBuilder.BuildTarget.Bzr as Bzr
import qualified Debian.AutoBuilder.BuildTarget.Uri as Uri
import qualified Debian.AutoBuilder.Params as P
import qualified Debian.AutoBuilder.Spec as S
import Debian.AutoBuilder.Tgt (Tgt(Tgt))
import Debian.Repo.Monad (AptIOT)
import System.Unix.Progress (qPutStrLn)
import Text.Regex(matchRegex, mkRegex)

readSpec :: P.CacheRec -> String -> AptIOT IO Tgt
readSpec cache text =
    qPutStrLn (" " ++ text) >>
    case text of
            'a':'p':'t':':' : target -> Tgt <$> Apt.prepare cache target
            'd':'a':'r':'c':'s':':' : target -> Tgt <$> lift (Darcs.prepare cache target)
            'd':'e':'b':'-':'d':'i':'r':':' : target ->
                parsePair target >>= \ (upstream, debian) -> Tgt <$> DebDir.prepare cache upstream debian
            'c':'d':':' : dirAndTarget ->
                do let (subdir, target) = second tail (break (== ':') dirAndTarget)
                   readSpec cache target >>= \ t -> Tgt <$> Cd.prepare cache subdir t
            'd':'i':'r':':' : target -> Tgt <$> Dir.prepare cache target
            'd':'e':'b':'i':'a':'n':'i':'z':'e':':' : target -> Tgt <$> Debianize.prepare cache target
            'h':'a':'c':'k':'a':'g':'e':':' : target -> Tgt <$> Hackage.prepare cache target
            'h':'g':':' : target -> Tgt <$> Hg.prepare cache target
            'q':'u':'i':'l':'t':':' : target ->
                parsePair target >>= \ (base, patch) -> Tgt <$> Quilt.prepare cache base patch
            's':'o':'u':'r':'c':'e':'d':'e':'b':':' : target ->
                readSpec cache target >>= \ t -> Tgt <$> SourceDeb.prepare cache t
            's':'v':'n':':' : target -> Tgt <$> Svn.prepare cache target
            't':'l':'a':':' : target -> Tgt <$> Tla.prepare cache target
            'b':'z':'r':':' : target -> Tgt <$> Bzr.prepare cache target
            'u':'r':'i':':' : target -> Tgt <$> Uri.prepare cache target
            'p':'r':'o':'c':':' : target ->
                readSpec cache target >>= \ t -> Tgt <$> Proc.prepare cache t
            _ -> fail ("Error in target specification: " ++ text)
    where
      parsePair :: String -> AptIOT IO (Tgt, Tgt)
      parsePair text =
          case match "\\(([^)]*)\\):\\(([^)]*)\\)" text of
            Just [baseName, patchName] ->
                do a <- readSpec cache baseName
                   b <- readSpec cache patchName
                   return (a, b)
            _ -> error ("Invalid spec name: " ++ text)
      match = matchRegex . mkRegex
      -- addTargetName = failing (\ msgs -> Failure (("Target " ++ text ++ "failed") : msgs)) Success

readSpec' :: P.CacheRec -> S.Spec -> AptIOT IO Tgt
readSpec' cache spec =
    qPutStrLn (" " ++ show spec) >>
    case spec of
      S.Apt dist package version -> Tgt <$> Apt.prepare' cache dist package version
      S.Darcs uri tag -> Tgt <$> lift (Darcs.prepare' cache uri tag)
      S.DebDir upstream debian ->
          readSpec' cache upstream >>= \ upstream' ->
          readSpec' cache debian >>= \ debian' ->
          (Tgt :: DebDir.DebDir -> Tgt) <$> (DebDir.prepare cache upstream' debian')
      S.Cd dir spec' -> Cd.prepare' cache dir spec'
      S.Dir path -> Tgt <$> Dir.prepare cache path
      S.Debianize package version -> Tgt <$> Debianize.prepare' cache package version
      S.Hackage package version -> Tgt <$> Hackage.prepare' cache package version
      S.Hg string -> Tgt <$> Hg.prepare cache string
      S.Proc spec' ->
          readSpec' cache spec' >>= \ tgt ->
          Tgt <$> Proc.prepare cache tgt
      S.Quilt base patches ->
          readSpec' cache base >>= \ base' ->
          readSpec' cache patches >>= \ patches' ->
          Tgt <$> Quilt.prepare cache base' patches'
      S.SourceDeb spec' ->
          readSpec' cache spec' >>= \ tgt ->
          Tgt <$> SourceDeb.prepare cache tgt
      S.Svn uri -> Tgt <$> Svn.prepare cache uri
      S.Tla string -> Tgt <$> Tla.prepare cache string
      S.Bzr string -> Tgt <$> Bzr.prepare cache string
      S.Uri uri sum -> Tgt <$> Uri.prepare' cache uri sum

targetDocumentation :: String
targetDocumentation =
    "TARGET TYPES\n\nEach argument to --target describes a technique for obtaining\n" ++
    "the source code used to build a target.  The following target types are available:\n\n" ++
    concat (intersperse "\n\n" $
            map (concat . intersperse "\n  ")
            [ [ "dir:<path> - A target of this form simply uses whatever it finds on"
              , "the local machine at the given path as the debian source tree."
              , "Packages built using this targets are not allowed to be uploaded"
              , "since they include no revision control information." ]
            , Apt.documentation
            , Cd.documentation
            , Darcs.documentation
            , DebDir.documentation
            , Debianize.documentation
            , Hackage.documentation
            , Hg.documentation
            , Proc.documentation
            , Quilt.documentation
            , SourceDeb.documentation
            , Svn.documentation
            , Tla.documentation
            , Uri.documentation ])
