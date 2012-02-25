-- |Data type representing a method for retrieving the source code of a package specification.
module Debian.AutoBuilder.Types.RetrieveMethod
    ( RetrieveMethod(..)
    ) where

-- |Represents only the data resulting from parsing the spec string (which is going away)
data RetrieveMethod
    = Apt String String (Maybe String)
    | Bzr String
    | Cd FilePath RetrieveMethod
    | Darcs String (Maybe String)
    | DebDir RetrieveMethod RetrieveMethod
    | Debianize String (Maybe String)
    | Dir FilePath
    | Hackage String (Maybe String)
    | Hg String
    | Proc RetrieveMethod
    | Quilt RetrieveMethod RetrieveMethod
    | SourceDeb RetrieveMethod
    | Svn String
    | Tla String
    | Twice RetrieveMethod
    | Uri String String
    deriving (Read, Show, Eq, Ord)
