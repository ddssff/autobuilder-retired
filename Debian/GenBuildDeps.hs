-- |Figure out the dependency relation between debianized source
-- directories.  The code to actually solve these dependency relations
-- for a particular set of binary packages is in Debian.Repo.Dependency.
module Debian.GenBuildDeps 
    ( DepInfo
    , SrcPkgName(..)
    , BinPkgName(..)
    -- * Preparing dependency info
    , buildDependencies
    , RelaxInfo(..)
    , relaxDeps
    -- * Using dependency info
    , BuildableInfo(..)
    , buildable
    , compareSource
    -- * Obsolete?
    , orderSource
    , genDeps
    , failPackage
    , getSourceOrder
    ) where

import		 Control.Monad (filterM)
import		 Debian.Control
import		 Data.Graph (Graph,buildG,topSort,reachable, transposeG, vertices, edges)
import		 Data.List
import qualified Data.Map as Map
import		 Data.Maybe
import qualified Data.Set as Set
import		 Debian.Relation
import		 Extra.Either (concatEithers)
import		 System.Directory (getDirectoryContents, doesFileExist)
import		 System.IO

newtype SrcPkgName = SrcPkgName PkgName deriving (Show, Eq)
newtype BinPkgName = BinPkgName PkgName deriving (Show, Eq)

-- |This type describes the build dependencies of a source package.
type DepInfo = (SrcPkgName,	-- source package name
                Relations,	-- dependency relations
                [BinPkgName])	-- binary package names

-- |Return the dependency info for a source package with the given dependency relaxation.
-- |According to debian policy, only the first paragraph in debian\/control can be a source package
-- <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-sourcecontrolfiles>
buildDependencies :: Control -> Either String DepInfo
buildDependencies (Control []) = error "Control file seems to be empty"
buildDependencies (Control (source:binaries)) =
    either (Left . concat) (\ deps -> Right (sourcePackage, deps, bins)) deps
    where
      sourcePackage = maybe (error "First Paragraph in control file lacks a Source field") SrcPkgName $ assoc "Source" source
      -- The raw list of build dependencies for this package
      deps = either Left (Right . concat) (concatEithers [buildDeps, buildDepsIndep])
      buildDeps =
          case assoc "Build-Depends" source of
            Just v -> either (\ e -> Left ("Error parsing Build-Depends for" ++ show sourcePackage ++ ": " ++ show e)) Right (parseRelations v)
            _ -> Right []
      buildDepsIndep =
          case assoc "Build-Depends-Indep" source of
            (Just v) -> either (\ e -> Left ("Error parsing Build-Depends-Indep for" ++ show sourcePackage ++ ": " ++ show e)) Right (parseRelations v)
            _ -> Right []
      bins = mapMaybe lookupPkgName binaries
      lookupPkgName :: Paragraph -> Maybe BinPkgName
      lookupPkgName p = maybe Nothing (Just . BinPkgName) (assoc "Package" p)

-- |Specifies build dependencies that should be ignored during the build
-- decision.  If the pair is (BINARY, Nothing) it means the binary package
-- BINARY should always be ignored when deciding whether to build.  If the
-- pair is (BINARY, Just SOURCE) it means that binary package BINARY should
-- be ignored when deiciding whether to build package SOURCE.
newtype RelaxInfo = RelaxInfo [(BinPkgName, Maybe SrcPkgName)] deriving (Show)

-- |Remove any dependencies that are designated \"relaxed\" by relaxInfo.
relaxDeps :: RelaxInfo -> [DepInfo] -> [DepInfo]
relaxDeps relaxInfo deps =
    map (relaxDep relaxInfo) deps
    where
      relaxDep :: RelaxInfo -> DepInfo -> DepInfo
      relaxDep relaxInfo (sourceName, relations, binaryNames) =
          (sourceName, filteredDependencies, binaryNames)
          where
            -- Discard any dependencies not on the filtered package name list.  If
            -- this results in an empty list in an or-dep the entire dependency can
            -- be discarded.
            filteredDependencies :: Relations
            filteredDependencies = filter (/= []) (map (filter keepDep) relations)
            keepDep :: Relation -> Bool
            keepDep (Rel name _ _) = not (elem (BinPkgName name) ignored)
            -- Binary packages to be ignored wrt this source package's build decision
            ignored = ignoredForSourcePackage sourceName relaxInfo
            -- Return a list of binary packages which should be ignored for this
            -- source package.
            ignoredForSourcePackage :: SrcPkgName -> RelaxInfo -> [BinPkgName]
            ignoredForSourcePackage source (RelaxInfo pairs) =
                map fst . filter (maybe True (== source) . snd) $ pairs
                -- concat . map binaries . catMaybes . map snd . filter (\ (_, x) -> maybe True (== source) x) $ pairs

data BuildableInfo a
    = BuildableInfo 
      { readyTriples :: [(a, [a], [a])]
      , allBlocked :: [a]
      }
    | CycleInfo
      { depPairs :: [(a, a)] }

-- |Given an ordering function representing the dependencies on a
-- list of packages, return a triple: One ready package, the packages
-- that depend on the ready package directly or indirectly, and all
-- the other packages.
buildable :: Show a => (a -> a -> Ordering) -> [a] -> BuildableInfo a
buildable cmp packages =
    -- Find all packages which can't reach any other packages in the
    -- graph of the "has build dependency" relation.
    case partition (\ x -> reachable hasDep x == [x]) verts of
      -- None of the packages are buildable, return information
      -- about how to break this build dependency cycle.
      ([], _) -> CycleInfo {depPairs = map ofEdge (cycleEdges hasDep)}
      -- We have some buildable packages, return them along with
      -- the list of packages each one directly blocks
      (allReady, blocked) ->
          BuildableInfo 
          { readyTriples = map (makeTriple blocked allReady) allReady,
            allBlocked = map ofVertex blocked }
    where
      makeTriple blocked ready thisReady =
          let otherReady = filter (/= thisReady) ready
              (directlyBlocked, otherBlocked) = partition (\ x -> elem x (reachable isDep thisReady)) blocked in
          (ofVertex thisReady, map ofVertex directlyBlocked, map ofVertex (otherReady ++ otherBlocked))
      --allDeps x = (ofVertex x, map ofVertex (filter (/= x) (reachable hasDep x)))
      isDep = buildG (0, length packages - 1) edges'
      edges' = map (\ (a, b) -> (b, a)) edges
      hasDep = buildG (0, length packages - 1) edges
      edges :: [(Int, Int)]
      edges =
          nub (foldr f [] (tails vertPairs))
          where f [] edges = edges
                f (x : xs) edges = catMaybes (map (toEdge x) xs) ++ edges
                toEdge (xv, xa) (yv, ya) =
                    case cmp xa ya of
                      EQ -> Nothing
                      LT -> Just (yv, xv)
                      GT -> Just (xv, yv)
      ofEdge (a, b) = (ofVertex a, ofVertex b)
      ofVertex n = fromJust (Map.findWithDefault Nothing n (Map.fromList (zip [0..] (map Just packages))))
      verts :: [Int]
      verts = map fst vertPairs
      vertPairs = zip [0..] packages

{-
-- |Given an ordering function representing the dependencies on a
-- list of packages, return a triple: One ready package, the packages
-- that depend on the ready package directly or indirectly, and all
-- the other packages.
buildable :: Show a => (a -> a -> Ordering) -> [a] -> Either [(a, a)] (a, [a], [a])
buildable cmp packages =
    case partition (\ x -> reachable hasDep x == [x]) verts of
      ([], _) -> Left (map ofEdge (cycleEdges hasDep))
      (thisReady : moreReady, allBlocked) ->
          let (deps, other) = partition (\ x -> elem x (reachable isDep thisReady)) allBlocked in
          Right (ofVertex thisReady, map ofVertex deps, map ofVertex (moreReady ++ other))
    where
      allDeps x = (ofVertex x, map ofVertex (filter (/= x) (reachable hasDep x)))
      isDep = buildG (0, length packages - 1) edges'
      edges' = map (\ (a, b) -> (b, a)) edges
      hasDep = buildG (0, length packages - 1) edges
      edges :: [(Int, Int)]
      edges =
          nub (foldr f [] (tails vertPairs))
          where f [] edges = edges
                f (x : xs) edges = catMaybes (map (toEdge x) xs) ++ edges
                toEdge (xv, xa) (yv, ya) =
                    case cmp xa ya of
                      EQ -> Nothing
                      LT -> Just (yv, xv)
                      GT -> Just (xv, yv)
      ofEdge (a, b) = (ofVertex a, ofVertex b)
      ofVertex n = fromJust (Map.findWithDefault Nothing n (Map.fromList (zip [0..] (map Just packages))))
      verts :: [Int]
      verts = map fst vertPairs
      vertPairs = zip [0..] packages
-}

cycleEdges g =
    filter (`elem` (edges g))
               (Set.toList (Set.intersection
                            (Set.fromList (closure g))
                            (Set.fromList (closure (transposeG g)))))
    where
      closure g = concat (map (\ v -> (map (\ u -> (v, u)) (reachable g v))) (vertices g))
      --self (a, b) = a == b
      --distrib = concat . map (\ (n, ms) -> map (\ m -> (n, m)) ms) 
      --swap (a, b) = (b, a)

-- | Remove any packages which can't be built given that a package has failed.
failPackage :: Eq a => (a -> a -> Ordering) -> a -> [a] -> ([a], [a])
failPackage compare failed packages =
    let graph = buildGraph compare packages in
    let root = elemIndex failed packages in
    let victims = maybe [] (map (fromJust . vertex) . reachable graph) root in
    partition (\ x -> not . elem x $ victims) packages
    where
      vertex n = Map.findWithDefault Nothing n vertexMap
      vertexMap = Map.fromList (zip [0..] (map Just packages))

-- | Given a list of packages, sort them according to their apparant
-- build dependencies so that the first element doesn't depend on any
-- of the other packages.
orderSource :: (a -> a -> Ordering) -> [a] -> [a]
orderSource compare packages =
    map (fromJust . vertex) (topSort graph)
    where
      graph = buildGraph compare packages
      vertex n = Map.findWithDefault Nothing n vertexMap
      vertexMap = Map.fromList (zip [0..] (map Just packages))

-- | Build a graph with the list of packages as its nodes and the
-- build dependencies as its edges.
buildGraph :: (a -> a -> Ordering) -> [a] -> Graph
buildGraph compare packages =
    let edges = someEdges (zip packages [0..]) in
    buildG (0, length packages - 1) edges
    where
      someEdges [] = []
      someEdges (a : etc) = aEdges a etc ++ someEdges etc
      aEdges (ap, an) etc =
          concat (map (\ (bp, bn) ->
                           case compare ap bp of
                                       LT -> [(an, bn)]
                                       GT -> [(bn, an)]
                                       EQ -> []) etc)

-- |This is a nice start. It ignores circular build depends and takes
-- a pretty simplistic approach to 'or' build depends. However, I
-- think this should work pretty nicely in practice.
compareSource :: DepInfo -> DepInfo -> Ordering
compareSource (_, depends1, bins1) (_, depends2, bins2)
    | any (\rel -> isJust (find (checkPackageNameReq rel) bins2)) (concat depends1) = GT
    | any (\rel -> isJust (find (checkPackageNameReq rel) bins1)) (concat depends2) = LT
    | otherwise = EQ
    where
      checkPackageNameReq :: Relation -> BinPkgName -> Bool
      checkPackageNameReq (Rel rPkgName _ _) (BinPkgName bPkgName) = rPkgName == bPkgName 

-- |Return the dependency info for a list of control files.
genDeps :: [FilePath] -> IO (Either String [DepInfo])
genDeps controlFiles =
    mapM genDep' controlFiles >>=
    return . either (Left . concat) (Right . orderSource compareSource) . concatEithers
    where
      genDep' :: FilePath -> IO (Either String DepInfo)
      genDep' controlPath = parseControlFromFile controlPath >>=
                            return . either (Left . show) buildDependencies

-- |One example of how to tie the below functions together. In this
-- case 'fp' is the path to a directory that contains a bunch of
-- checked out source packages. The code will automatically look for
-- fp\/*\/debian\/control. It returns a list with the packages in the
-- order they should be built.
getSourceOrder :: FilePath -> IO (Either String [SrcPkgName])
getSourceOrder fp =
    findControlFiles fp >>=
    genDeps >>=
    return . either Left (Right . map ( \(pkgName,_,_) -> pkgName) . orderSource compareSource)
    where
      -- Return a list of the files that look like debian\/control.
      findControlFiles :: FilePath -> IO [FilePath]
      findControlFiles root =
          getDirectoryContents root >>=
          mapM (\ x -> return $ root ++ "/" ++ x ++ "/debian/control") >>=
          filterM doesFileExist


assoc :: String -> Paragraph -> Maybe String
assoc name fields = maybe Nothing (\ (Field (_, v)) -> Just (stripWS v)) (lookupP name fields)
