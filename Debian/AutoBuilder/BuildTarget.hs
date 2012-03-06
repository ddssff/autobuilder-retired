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
import qualified Debian.AutoBuilder.BuildTarget.Bzr as Bzr
import qualified Debian.AutoBuilder.BuildTarget.Uri as Uri
import qualified Debian.AutoBuilder.BuildTarget.Twice as Twice
import qualified Debian.AutoBuilder.Types.CacheRec as P
import Debian.AutoBuilder.Types.Download
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo (OSImage)
import Debian.Repo.Monad (AptIOT)
import System.Unix.QIO (q12)

-- | Given a RetrieveMethod, perform the retrieval and return the result.
retrieve :: OSImage -> P.CacheRec -> P.RetrieveMethod -> [P.PackageFlag] -> AptIOT IO Download
retrieve buildOS cache spec flags =
    q12 (" " ++ show spec) $     
     case spec of
      P.Apt dist package -> Apt.prepare cache spec flags dist package
      P.Bzr string -> Bzr.prepare cache spec flags string
      P.Cd dir spec' ->
          retrieve buildOS cache spec' [] >>= \ t ->
          Cd.prepare cache spec flags dir t
      P.Darcs uri -> lift (Darcs.prepare cache spec flags uri)
      P.DebDir upstream debian ->
          do upstream' <- retrieve buildOS cache upstream []
             debian' <- retrieve buildOS cache debian []
             DebDir.prepare cache spec flags upstream' debian'
      P.Debianize package -> Debianize.prepare cache spec flags package
      P.Dir path -> Dir.prepare cache spec flags path
      P.Hackage package -> Debianize.prepareHackage cache spec flags package
      P.Hg string -> Hg.prepare cache spec flags string
      P.Proc spec' ->
          retrieve buildOS cache spec' [] >>= \ t ->
          Proc.prepare cache spec flags buildOS t
      P.Quilt base patches ->
          retrieve buildOS cache base [] >>= \ base' ->
          retrieve buildOS cache patches [] >>= \ patches' ->
          Quilt.prepare cache spec flags base' patches'
      P.SourceDeb spec' ->
          retrieve buildOS cache spec' [] >>= \ t ->
          SourceDeb.prepare cache spec flags t
      P.Svn uri -> Svn.prepare cache spec flags uri
      P.Tla string -> Tla.prepare cache spec flags string
      P.Twice base -> retrieve buildOS cache base [] >>= \ t ->
                      Twice.prepare spec flags t
      P.Uri uri sum -> Uri.prepare cache spec flags uri sum

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
