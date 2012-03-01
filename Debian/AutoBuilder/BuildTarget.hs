{-# LANGUAGE RankNTypes #-}
module Debian.AutoBuilder.BuildTarget
    ( retrieve
    , targetDocumentation
    ) where

import Control.Monad.Trans (lift)
import Data.List (intersperse)
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
import qualified Debian.AutoBuilder.BuildTarget.Temp as T
import qualified Debian.AutoBuilder.BuildTarget.Bzr as Bzr
import qualified Debian.AutoBuilder.BuildTarget.Uri as Uri
import qualified Debian.AutoBuilder.BuildTarget.Twice as Twice
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.PackageFlag as P
import qualified Debian.AutoBuilder.Types.RetrieveMethod as R
import Debian.Repo (OSImage)
import Debian.Repo.Monad (AptIOT)
import System.Unix.QIO (q12)

-- | Given a RetrieveMethod, perform the retrieval and return the result.
retrieve :: OSImage -> P.CacheRec -> [P.PackageFlag] -> R.RetrieveMethod -> AptIOT IO T.Download
retrieve buildOS cache flags spec =
    q12 (" " ++ show spec) $     
     (case spec of
      R.Apt dist package flags -> Apt.prepare cache dist package flags spec
      R.Darcs uri flags -> lift (Darcs.prepare cache uri flags spec)
      R.DebDir upstream debian ->
          do upstream' <- retrieve buildOS cache [] upstream
             debian' <- retrieve buildOS cache [] debian
             DebDir.prepare cache upstream' debian' spec
      R.Cd dir spec' ->
          retrieve buildOS cache [] spec' >>= \ t ->
          Cd.prepare cache dir t spec
      R.Dir path -> Dir.prepare cache path spec
      R.Debianize package version -> Debianize.prepare cache flags package version spec
      R.Hackage package version -> Debianize.prepareHackage cache package version spec
      R.Hg string -> Hg.prepare cache string spec
      R.Proc spec' ->
          retrieve buildOS cache [] spec' >>= \ t ->
          Proc.prepare cache t spec buildOS
      R.Quilt base patches ->
          retrieve buildOS cache [] base >>= \ base' ->
          retrieve buildOS cache [] patches >>= \ patches' ->
          Quilt.prepare cache base' patches' spec
      R.SourceDeb spec' ->
          retrieve buildOS cache [] spec' >>= \ t ->
          SourceDeb.prepare cache t spec
      R.Svn uri -> Svn.prepare cache uri spec
      R.Tla string -> Tla.prepare cache string spec
      R.Bzr string -> Bzr.prepare cache string spec
      R.Uri uri sum -> Uri.prepare cache uri sum spec
      R.Twice base -> retrieve buildOS cache [] base >>= \ t ->
                      Twice.prepare t spec )

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
