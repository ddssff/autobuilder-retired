-- |Data type representing the pure data in a package specification - it describes
-- how to obtain the source code for the package, but doesn't contain any of the
-- results of perform the IO operations to actually obtain the source.
module Debian.AutoBuilder.Spec
    ( Spec(..)
    ) where

-- |Represents only the data resulting from parsing the spec string (which is going away)
data Spec
    = Apt String String (Maybe String)
    | Bzr String
    | Cd FilePath Spec
    | Darcs String (Maybe String)
    | DebDir Spec Spec
    | Debianize String (Maybe String)
    | Dir FilePath
    | Hackage String (Maybe String)
    | Hg String
    | Proc Spec
    | Quilt Spec Spec
    | SourceDeb Spec
    | Svn String
    | Tla String
    | Twice Spec
    | Uri String String
    deriving (Read, Show, Eq, Ord)
