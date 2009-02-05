module Debian.Relation.Common where

-- Standard GHC Modules

import Data.List
import Text.ParserCombinators.Parsec

-- Local Modules

import Debian.Version

-- Datatype for relations

type PkgName = String

type Relations = AndRelation
type AndRelation = [OrRelation]
type OrRelation = [Relation]

data Relation = Rel PkgName (Maybe VersionReq) (Maybe ArchitectureReq)
		deriving Eq


class ParseRelations a where
    -- |'parseRelations' parse a debian relation (i.e. the value of a
    -- Depends field). Return a parsec error or a value of type
    -- 'Relations'
    parseRelations :: a -> Either ParseError Relations


instance Show Relation where
    show (Rel name ver arch) =
        name ++ maybe "" show ver ++ maybe "" show arch

instance Ord Relation where
    compare (Rel pkgName1 mVerReq1 _mArch1) (Rel pkgName2 mVerReq2 _mArch2) =
	case compare pkgName1 pkgName2 of
	     LT -> LT
	     GT -> GT
	     EQ -> compare mVerReq1 mVerReq2

data ArchitectureReq
    = ArchOnly [String]
    | ArchExcept [String]
      deriving Eq

instance Show ArchitectureReq where
    show (ArchOnly arch) = " [" ++ intercalate " " arch ++ "]"
    show (ArchExcept arch) = " [!" ++ intercalate " !" arch ++ "]"

data VersionReq
    = SLT DebianVersion
    | LTE DebianVersion
    | EEQ  DebianVersion
    | GRE  DebianVersion
    | SGR DebianVersion
      deriving Eq

instance Show VersionReq where
    show (SLT v) = " (<< " ++ show v ++ ")"
    show (LTE v) = " (<= " ++ show v ++ ")"
    show (EEQ v) = " (= " ++ show v ++ ")"
    show (GRE v) = " (>= " ++ show v ++ ")"
    show (SGR v) = " (>> " ++ show v ++ ")"

-- |@FIXME:@ This instance is currently incomplete and only handles the case
-- where two version requirements are equal.
instance Ord VersionReq where
    compare r1 r2 =
	if r1 == r2 
	   then EQ 
	   else
	case (r1, r2) of
	     (EEQ v1, EEQ v2) -> compare v1 v2
	     (a,b) -> error $ "Ord VersionReq does not handle (" ++ show a ++", "++ show b++")"


-- |Check if a version number satisfies a version requirement.
checkVersionReq :: Maybe VersionReq -> Maybe DebianVersion -> Bool
checkVersionReq Nothing _ = True
checkVersionReq _ Nothing = False
checkVersionReq (Just (SLT v1)) (Just v2) = v2 < v1
checkVersionReq (Just (LTE v1)) (Just v2) = v2 <= v1
checkVersionReq (Just (EEQ v1)) (Just v2) = v2 == v1
checkVersionReq (Just (GRE v1)) (Just v2) = v2 >= v1
checkVersionReq (Just (SGR v1)) (Just v2) = v2 > v1
