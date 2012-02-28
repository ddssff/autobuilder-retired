{-# LANGUAGE RankNTypes #-}
module Debian.AutoBuilder.BuildTarget
    ( retrieve
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
import qualified Debian.AutoBuilder.BuildTarget.Hg as Hg
import qualified Debian.AutoBuilder.BuildTarget.Proc as Proc
import qualified Debian.AutoBuilder.BuildTarget.Quilt as Quilt
import qualified Debian.AutoBuilder.BuildTarget.SourceDeb as SourceDeb
import qualified Debian.AutoBuilder.BuildTarget.Svn as Svn
import qualified Debian.AutoBuilder.BuildTarget.Tla as Tla
import qualified Debian.AutoBuilder.BuildTarget.Bzr as Bzr
import qualified Debian.AutoBuilder.BuildTarget.Uri as Uri
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.PackageFlag as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.AutoBuilder.Tgt (Tgt(Tgt))
import Debian.Repo.Monad (AptIOT)
import System.Unix.QIO (q12)

-- | Given a RetrieveMethod, perform the retrieval and return the result.
retrieve :: P.CacheRec -> [P.PackageFlag] -> R.RetrieveMethod -> AptIOT IO Tgt
retrieve cache flags spec =
    q12 (" " ++ show spec) $     
     (case spec of
      R.Apt dist package flags -> tgt <$> Apt.prepare cache dist package flags spec
      R.Darcs uri flags -> tgt <$> lift (Darcs.prepare cache uri flags spec)
      R.DebDir upstream debian ->
          do upstream' <- retrieve cache [] upstream
             debian' <- retrieve cache [] debian
             tgt <$> (DebDir.prepare cache upstream' debian' spec)
      R.Cd dir spec' ->
          retrieve cache [] spec' >>= \ t ->
          tgt <$> Cd.prepare cache dir t spec
      R.Dir path -> tgt <$> Dir.prepare cache path spec
      R.Debianize package version -> tgt <$> Debianize.prepare cache flags package version spec
      R.Hackage package version -> tgt <$> Debianize.prepareHackage cache package version spec
      R.Hg string -> tgt <$> Hg.prepare cache string spec
      R.Proc spec' ->
          retrieve cache [] spec' >>= \ t ->
          tgt <$> Proc.prepare cache t spec
      R.Quilt base patches ->
          retrieve cache [] base >>= \ base' ->
          retrieve cache [] patches >>= \ patches' ->
          tgt <$> Quilt.prepare cache base' patches' spec
      R.SourceDeb spec' ->
          retrieve cache [] spec' >>= \ t ->
          tgt <$> SourceDeb.prepare cache t spec
      R.Svn uri -> tgt <$> Svn.prepare cache uri spec
      R.Tla string -> tgt <$> Tla.prepare cache string spec
      R.Bzr string -> tgt <$> Bzr.prepare cache string
      R.Uri uri sum -> tgt <$> Uri.prepare cache uri sum spec
      R.Twice {} -> error "Unimplemented: Twice")
    where
      -- If any flags were passed in we want to build a Top, otherwise a Tgt
      tgt :: forall a. BuildTarget a => a -> Tgt
      tgt x = Tgt x

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
            , Debianize.documentationHackage
            , Hg.documentation
            , Proc.documentation
            , Quilt.documentation
            , SourceDeb.documentation
            , Svn.documentation
            , Tla.documentation
            , Uri.documentation ])
