{-# LANGUAGE RankNTypes #-}
module Debian.AutoBuilder.BuildTarget
    ( readSpec
    , targetDocumentation
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)
import Data.List (intersperse)
import Debian.AutoBuilder.BuildTarget.Common (BuildTarget)
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
import Debian.AutoBuilder.Tgt (Tgt(Tgt, Top))
import Debian.Repo.Monad (AptIOT)
import Debian.Repo.Types (q12)

readSpec :: P.CacheRec -> [P.PackageFlag] -> S.Spec -> AptIOT IO Tgt
readSpec cache flags spec =
    q12 (" " ++ show spec) $     
     (case spec of
      S.Apt dist package version -> tgt <$> Apt.prepare cache dist package version
      S.Darcs uri tag -> tgt <$> lift (Darcs.prepare cache uri tag)
      S.DebDir upstream debian ->
          readSpec cache [] upstream >>= \ upstream' ->
          readSpec cache [] debian >>= \ debian' ->
          tgt <$> (DebDir.prepare cache upstream' debian')
      S.Cd dir spec' ->
          readSpec cache [] spec' >>= \ t ->
          tgt <$> Cd.prepare cache dir t
      S.Dir path -> tgt <$> Dir.prepare cache path
      S.Debianize package version -> tgt <$> Debianize.prepare cache flags package version
      S.Hackage package version -> tgt <$> Hackage.prepare cache package version
      S.Hg string -> tgt <$> Hg.prepare cache string
      S.Proc spec' ->
          readSpec cache [] spec' >>= \ t ->
          tgt <$> Proc.prepare cache t
      S.Quilt base patches ->
          readSpec cache [] base >>= \ base' ->
          readSpec cache [] patches >>= \ patches' ->
          tgt <$> Quilt.prepare cache base' patches'
      S.SourceDeb spec' ->
          readSpec cache [] spec' >>= \ t ->
          tgt <$> SourceDeb.prepare cache t
      S.Svn uri -> tgt <$> Svn.prepare cache uri
      S.Tla string -> tgt <$> Tla.prepare cache string
      S.Bzr string -> tgt <$> Bzr.prepare cache string
      S.Uri uri sum -> tgt <$> Uri.prepare cache uri sum
      S.Twice {} -> error "Unimplemented: Twice")
    where
      -- If any flags were passed in we want to build a Top, otherwise a Tgt
      tgt :: forall a. (Show a, BuildTarget a) => a -> Tgt
      tgt x = case flags of
                [] -> Tgt x
                _ -> Top flags x

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
