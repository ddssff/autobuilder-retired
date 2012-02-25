-- |Data type representing a method for retrieving the source code of a package specification.
module Debian.AutoBuilder.Types.RetrieveMethod
    ( RetrieveMethod(..)
    ) where

import Debian.AutoBuilder.Types.PackageFlag (AptFlag, CabalFlag, DarcsFlag)

-- |Represents only the data resulting from parsing the spec string (which is going away)
data RetrieveMethod
    = Apt String String [AptFlag]
    | Bzr String
    | Cd FilePath RetrieveMethod
    | Darcs String [DarcsFlag]
    | DebDir RetrieveMethod RetrieveMethod
    | Debianize String [CabalFlag]
    | Dir FilePath
    | Hackage String [CabalFlag]
    | Hg String
    | Proc RetrieveMethod
    | Quilt RetrieveMethod RetrieveMethod
    | SourceDeb RetrieveMethod
    | Svn String
    | Tla String
    | Twice RetrieveMethod
    | Uri String String
    deriving (Read, Show, Eq, Ord)
