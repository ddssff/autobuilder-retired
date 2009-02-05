{-# LANGUAGE ScopedTypeVariables, ScopedTypeVariables #-}
-- |This dependency solver determines which binary packages to install 
-- in order to satisfy a set of dependency relations.  It uses a brute
-- force method, but tweaked to the point where it is usually able to
-- complete on real-world inputs.
--
-- Author: David Fox <dsf@seereason.com>
module Debian.Repo.Dependencies
    ( simplifyRelations
    , solutions
    , testArch
    ) where

import		 Debian.Control()
import qualified Debian.Control.String as S()
import		 Debian.Repo.Types
import		 Data.List
import qualified Data.Map as Map
import		 Data.Maybe
import		 Debian.Relation
import		 Extra.List (cartesianProduct)

type Excuse = String

type ProvidesMap = Map.Map String [BinaryPackage]

-- |A SimpleRelation just specifies a particular version of a package.
-- The Nothing relation is always satisified.
type SimpleRelation = Maybe PkgVersion

-- |Each element is an or-list of specific package versions.
type SimpleRelations = [[SimpleRelation]]                     

instance PackageVersion BinaryPackage where
    pkgName = packageName . packageID
    pkgVersion = packageVersion . packageID

-- Does this relation apply to this architecture?
testArch :: Arch -> Relation -> Bool
testArch _ (Rel _ _ Nothing) = True
testArch architecture (Rel _ _ (Just (ArchOnly archList))) = elem architecture (map Binary archList)
testArch architecture (Rel _ _ (Just (ArchExcept archList))) = not (elem architecture (map Binary archList))

-- |Turn the expressive inequality style relations to a set of simple
-- equality relations on only the packages in the available list.
simplifyRelations :: [BinaryPackage]
                  -> Relations
                  -> [String]		-- ^ Given several alternative packages which satisfy
				        -- the relation, sort by name in this order.
                  -> Arch		-- ^ The build architecture
                  -> SimpleRelations
simplifyRelations available relations preferred arch =
    -- Sort the or-relations so that
    --  1. the packages named in the preferred list appear before other packages,
    --  2. the newest version appear before the older
    map (sortBy prefOrder . reverse . sort) relationsSimplified
    where
      relationsSimplified = expandVirtual nameMap providesMap relationsOfArch
          where
            nameMap = listMap (map (\ package -> (packageName (packageID package), package)) available)
            providesMap =
                listMap (concat (map (\ package -> 
                                      let names = packageName (packageID package) : map provides (pProvides package) in
                                      map (\ name -> (name, package)) names) available))
            provides [Rel name Nothing Nothing] = name
            provides bzzt = error ("Invalid relation in Provides: " ++ show bzzt)
            relationsOfArch = filter (/= []) (map (nub . filter (testArch arch)) relations)
      prefOrder a b = compare (isPreferred a) (isPreferred b)
          where isPreferred = maybe False (flip elem preferred . getName)

-- |Replace all relations by sets of equality relations on the exact
-- package versions which are actually available in the current
-- environment and satisfy the original relation.
expandVirtual :: ProvidesMap -> ProvidesMap -> Relations -> SimpleRelations
expandVirtual nameMap providesMap relations =
    map (nub . concat . map expand) relations
    where
      -- A relation with no version or architecture requirement
      -- can be satisfied by a provides or a real package.
      expand (Rel name Nothing Nothing) = map eqRel (Map.findWithDefault [] name providesMap)
      expand rel@(Rel name _ _) = map eqRel (filter (satisfies rel) (Map.findWithDefault [] name nameMap))
      eqRel :: BinaryPackage -> SimpleRelation
      eqRel package =
          Just (PkgVersion {getName = packageName p, getVersion = packageVersion p})
          where p = packageID package
      -- Does this package satisfy the relation?
      satisfies :: PackageVersion a => Relation -> a -> Bool
      satisfies rel pkg = testRel (pkgVersion pkg) rel

-- Does this version satisfy the relation?
testRel _ (Rel _ Nothing _) = True
testRel ver1 (Rel _ (Just (LTE ver2)) _) = not (compare ver1 ver2 == GT)
testRel ver1 (Rel _ (Just (GRE ver2)) _) = not (compare ver1 ver2 == LT)
testRel ver1 (Rel _ (Just (SLT ver2)) _) = compare ver1 ver2 == LT
testRel ver1 (Rel _ (Just (EEQ ver2)) _) = compare ver1 ver2 == EQ
testRel ver1 (Rel _ (Just (SGR ver2)) _) = compare ver1 ver2 == GT

-- |Given a root and a dependency list, return a list of possible
-- solutions to the dependency set.  Each solution is a list of
-- package versions which satisfy all the dependencies.  Note that if
-- a package is mentioned in two different clauses of the dependency
-- list, both clauses must be satisfied:
--
-- * a (>= 3.0), a (<< 4.0) | b (>= 2.0), c (>= 1.0) becomes
--
-- * a (>= 3.0), a (<< 4.0), c (>= 1.0) OR a (>= 3.0), b (>= 2.0), c (>= 1.0)
--
-- * [[a (>= 3.0)], [a (<< 4.0), b (>= 2.0)], [c (>= 1.0)]] becomes
--
-- * [[a (>= 3.0), a (<< 4.0), c (>= 1.0)], [a (>= 3.0), b (>= 2.0), c (>= 1.0)]]
--
-- So we can use each clause to eliminate packages which cannot
-- satisfy the dependency set.
solutions :: [BinaryPackage]	-- ^ The packages available to satisfy dependencies
          -> SimpleRelations	-- ^ The dependency relations to be satisfied
          -> Int		-- ^ Give up after this many solutions are computed
          -> (Either String [(Int, [BinaryPackage])])
				-- ^ On success return the set of packages to install,
				-- and the solution's sequence number.  Also returns
				-- the modified list of dependency relations, with all
				-- inequalities replaced by equalities on the particular
				-- versions of each package which are available.
solutions available relations limit =
    -- Do any of the dependencies require packages that simply don't
    -- exist?  If so we don't have to search for solutions, there
    -- aren't any.
    case any (== []) relations of
      True -> Left "Unsatisfiable dependencies"
      False ->
          -- Turn the And-of-Ors dependency list into Or-of-And-of-Ands.
          -- Each element of the result represents a an alternative set of
          -- constraints which a solution must satisfy.  Each element of
          -- the alternative is a list of relations on a single package,
          -- all of which must be satisfied.
          let alternatives = map (map nub . groupByName) (cartesianProduct relations) in
          --let versions = map makeVersion available in
          -- Find a set of packages that satisfies the dependencies
          case solutions' 1 alternatives available of
            -- Add more information about the failure to the error message.
            Left message ->
                let results = map (testAlternative available) (take 20 alternatives) in
                let (errors :: [String]) = catMaybes (map (either Just (const Nothing)) results) in
                Left (message ++ "\n" ++ intercalate "\n" errors)
            Right x -> Right x
    where
      solutions' :: PackageVersion a => Int -> [[[SimpleRelation]]] -> [a] -> Either String [(Int, [a])]
      solutions' _ [] _ = Left "All candidate solutions failed"
      solutions' count (alternative : alternatives) available =
          if count > limit then
              Left ("No solutions found in first " ++ show limit ++ " candidates") else
              case testAlternative available alternative of
                Left _ ->
                    solutions' (count + 1) alternatives available
                Right solution ->
                    Right ((count, solution)
                          : either (const []) id (solutions' (count + 1) alternatives available))

-- |The alternative argument is a possible solution to the dependency
-- problem.  Each element of alternative represents the relations on a
-- particular package.  So each one needs to be satisfied for the
-- alternative to be satisfied.
testAlternative :: PackageVersion a => [a] -> [[SimpleRelation]] -> Either Excuse [a]
testAlternative available alternative =
    -- 
    if all (/= []) solution then
        Right (map head solution) else
        Left ("Couldn't satisfy these conditions:\n  " ++ 
              intercalate "\n  " (map show (mask (map (== []) solution) alternative)))
    where
      solution = map (testPackage available) alternative
      mask bits elems = map fst (filter snd (zip elems bits))

-- |Return the list of versions of a package that satisfy all of the
-- relations.
testPackage :: PackageVersion a => [a] -> [SimpleRelation] -> [a]
testPackage available rels = foldl satisfies available rels
    where
      -- Which of these packages satisfy the relation?
      satisfies :: PackageVersion a => [a] -> SimpleRelation -> [a]
      satisfies available Nothing = available
      satisfies available (Just pkg) = filter same available
          where same x = pkgName x == pkgName pkg && pkgVersion x == pkgVersion pkg

groupByName :: [SimpleRelation] -> [[SimpleRelation]]
groupByName relations =
    groupBy (\ a b -> compareNames a b == EQ) (sortBy compareNames relations)
    where compareNames a b = compare (maybe Nothing (Just . getName) a) (maybe Nothing (Just . getName) b)

-- Turn a list of (k, a) pairs into a map from k -> [a].
listMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
listMap pairs =
    foldl insertPair Map.empty pairs
    where insertPair m (k,a) = Map.insert k (a : (Map.findWithDefault [] k m)) m