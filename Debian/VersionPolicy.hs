-- |This module contains functions governing the assignment of version
-- numbers, patterned after Ubuntu version number policy:
--
-- (1) To distinguish packages which have been pulled out of one vendor's
-- repository to build for another, we add a vendor name and a build
-- number, so version "1.2-3" becomes "1.2-3vendorname1".  Subsequent
-- builds (perhaps due to changes in build dependencies) would be numbered
-- "1.2-3vendorname2" and so on.
--
-- (2) In addition, packages which are backported, (i.e. built for a
-- non-development release such as Debian etch or Ubuntu feisty) need
-- another tag to distinguish them from the version that would go into
-- the development release (sid or, as of this writing, heron.)  So if
-- we pulled the source for version "1.2-3vendorname4" out of our pool
-- to build for feisty, it would become "1.2-3vendorname4~feisty1".
--
-- (3) The first two policies combine if we
-- are building a package pulled directly from the other vendor into
-- our feisty pool, then "1.2-3" would become 1.2-3vendorname0~feisty1.
-- Subsequent builds of "1.2-3" (perhaps due to changes in build
-- dependencies) would get increasing build numbers,
-- "1.2-3vendorname0~feisty2" etc.
--
-- (4) If the original version number does not end with a digit, a "0" is
-- inserted before the vendor name to facilitate parsing of these tags.
--
-- (5) Finally, an additional tag format is supported for the benefit of
-- one autobuilder client, where before the vendor name an "r" and an
-- integer are inserted.
module Debian.VersionPolicy
    ( VersionTag
    , parseTag
    , getTag
    , dropTag
    , setTag
    , appendTag
    , tagCmp
    , tagMax
    , bumpTag
    , newTag
    , compareSourceAndDist
    ) where

import Debian.Version
import Text.Regex
import Data.List
import Data.Maybe

-- |We implement two types of version number tags.  One has the format
--   @r<releasenumber>vendor<buildnumber>~release<buildnumber>@
-- the other simply
--   @vendor<buildnumber>~release<buildnumber>@
-- There are notes from a meeting that explains why ReleaseTagBuild is
-- more future friendly, but it is also more ugly.
data VersionTag
    = VersionTag { extraNumber :: Maybe Int		-- The number following the "r" (do not
							-- use in new applications.)
                 , vendorTag :: (String, Int)		-- The vendor name and build number
                 , releaseTag :: Maybe (String, Int)	-- The release name and build number
                 } deriving (Show, Eq)

-- | Parse a Debian revision string (the portion of the version number
-- following the final dash) into a prefix and a VersionTag.
parseTag :: String -> DebianVersion -> (DebianVersion, Maybe VersionTag)
parseTag vendor version =
    let (e, v, r) = evr version in
    let (prefix, tag) =
            case r of
              Nothing -> (Nothing, Nothing)
              Just s ->
                  case matchRegex re s of
                    Nothing -> (Just s, Nothing)
                    Just [prefix1, prefix2, buildNo, "", _, _, _, _] ->
                        (Just (prefix1 ++ prefix2),
                         Just (VersionTag { extraNumber = Nothing
                                          , vendorTag = (vendor, read buildNo)
                                          , releaseTag = Nothing }))
                    Just [prefix1, prefix2, buildNo, _, releaseName, _, _, releaseNo] ->
                        (Just (prefix1 ++ prefix2),
                         Just (VersionTag { extraNumber = Nothing
                                          , vendorTag = (vendor, read buildNo)
                                          , releaseTag = Just (releaseName, read releaseNo) }))
                    Just result -> error $ "Internal error: " ++ show result in
    -- Try to parse the r5 from the end of the prefix.
    let (prefix', tag') =
            case (maybe Nothing (matchRegex extraRE) prefix, tag) of
              (Just [prefix1, prefix2, digits], Just tag) -> 
                  (Just (prefix1 ++ prefix2), Just (tag {extraNumber = Just (read digits)}))
              _ -> (prefix, tag) in
    (buildDebianVersion e v (if prefix' == Just "0" then Nothing else prefix'), tag')
    where
      re = mkRegex (prefixRE ++ digitsRE ++ vendorRE ++ "(" ++ releaseRE ++ ")?$")
      prefixRE = "^(.*[^0-9])?"
      digitsRE = "([0-9]+)"
      vendorRE = vendor ++ "([0-9]+)"
      releaseRE = "~(([^0-9]+)|(bpo[0-9]+\\+))([0-9]+)"
      extraRE = mkRegex "^(.*)([0-9]+)r([0-9]+)"

-- | The tag returned by splitTag
getTag :: String -> DebianVersion -> Maybe VersionTag
getTag vendor version = snd (parseTag vendor version)

-- | The prefix returned by splitTag
dropTag :: String -> DebianVersion -> DebianVersion
dropTag vendor version = fst (parseTag vendor version)

-- |Modify a version number by adding or changing the vendor tag.  The
-- result will be newer than the distVersion (the newest already
-- uploaded version.)  It will also be different from (though not
-- necessarily newer than) any of the elements of allVersions
setTag :: (String -> String)
       -- ^ The release name alias function.  As an example, this would map
       -- the repository etch to the tag bpo40+, as specified by Debian policy.
       -> String
       -- ^ The vendor tag
       -> Maybe String
       -- ^ The build release, or Nothing if this is a development release
       -> Maybe Int
       -- ^ Use old "r0vendor1" format
       -> Maybe DebianVersion
       -- ^ The newst version that is already present in the dist.  We need
       -- to generate a version number newer than this.
       -> [DebianVersion]
       -- ^ All the versions that are currently available in the pool.  The
       -- result must not be a member of this list.
       -> DebianVersion
       -- ^ The version number that appears in the newest change log
       -- entry of the source package.  The result will be this number
       -- with a version tag added.  If this version is older than the
       -- current version with its tag stripped, the package cannot be
       -- built.
       -> Either String DebianVersion
       -- ^ The modified version number
{-setTag alias vendor release extra distVersion allVersions sourceVersion =
    error ("vendor=" ++ show vendor ++ ", release=" ++ show release ++ ", extra=" ++ show extra ++ ", distVersion=" ++ show distVersion ++ ", allVersions=" ++ show allVersions ++ ", sourceVersion=" ++ show sourceVersion)-}
setTag alias vendor release extra distVersion allVersions sourceVersion =
    let oldTag = 
            case maybe Nothing (Just . parseTag vendor) distVersion of
              Nothing -> Right Nothing
              Just (distUpstreamVersion, distTag) ->
                  case compare sourceUpstreamVersion distUpstreamVersion of
                    LT -> Left ("Source version " ++ show sourceVersion ++
                                " is too old to trump uploaded version " ++ show distUpstreamVersion)
                    GT -> Right Nothing
                    EQ -> Right distTag in
    either Left (Right . appendTag alias sourceUpstreamVersion . Just . findAvailableTag . newTag) oldTag
    where
      -- The new tag must
      --  1) be newer than the old tag
      --  2) be at least as new as the tag in the source code changelog.
      --  3) have the appropriate releaseTag
      -- The oldTag may not have an appropriate releaseTag, because the
      -- distribution may have recently switched from development to
      -- non-development.  In that case we will have to bump the
      -- vendor build number, not the release build number, and then
      -- add the release tag.
      newTag Nothing =
          VersionTag {vendorTag = (vendor, 1), releaseTag = maybe Nothing (\ relName -> Just (relName, 1)) release, extraNumber = extra}
      newTag (Just distTag) =
          let distTag' = fixReleaseName release (bumpTag distTag) in
          let sourceTag' = maybe Nothing (Just . setReleaseName release) sourceTag in
          case tagMax [Just distTag', sourceTag'] of
            Nothing -> error $ "Internal error"
            Just tag -> tag
{-
          case (release, sourceTag, distTag') of
            (_, _, VersionTag {vendorTag = (distVendorName, distVendorBuild)})
                | distVendorName /= vendor -> 
                    error $ "Vendor name mismatch: " ++ show (vendor, distVendorName)
            (_, Just (VersionTag {vendorTag = (sourceVendorName, sourceVendorBuild)}), _)
                | sourceVendorName /= vendor -> 
                    error $ "Vendor name mismatch: " ++ show (vendor, sourceVendorName)
            (Just relName, _, VersionTag {releaseTag = Just (distRelName, distRelBuild)})
                | relName /= undefined ->
                    undefined

          let candidate1 = fixTag sourceTag in
          let candidate2 = setRelease release (bumpTag' distTag) in
          let candidate' = tagMax candidate 
                  case (tagMax [sourceTag, Just (bumpTag vendor release extra distTag)]) of
                    
          let candidate = maybe Nothing (Just . fixTag) (tagMax [sourceTag, Just (bumpTag vendor release extra distTag)]) in
          appendTag alias sourceUpstreamVersion (findAvailableTag candidate)
      -- Change a tag so it is appropriate for the release, adding or
      -- removing the release tag.
      fixTag tag = case (releaseTag tag, release) of 
                     (Just _, Nothing) -> tag {releaseTag = Nothing}
                     (Nothing, Just rel) -> tag {releaseTag = Just (rel, 1)}
                     (Just (oldRel, oldBuild), Just newRel) | oldRel /= newRel -> tag {releaseTag = Just (newRel, 1)}
                     (Just (_, build), Just rel) -> tag {releaseTag = Just (rel, build)}
-}
      (sourceUpstreamVersion, sourceTag) = parseTag vendor sourceVersion
      -- All the tags of existing packages whose upstream version matches the source
      allTags = catMaybes (map snd (filter (\ (v, _) -> v == sourceUpstreamVersion) (map (parseTag vendor) allVersions)))
      -- Repeatedly increment candidate until it differs from the
      -- elements of all.  Note that this is not the same as taking
      -- the maximum element and incrementing it, the value we want
      -- only needs to be not less than candidate.
      findAvailableTag :: VersionTag -> VersionTag
      findAvailableTag candidate =
          if elem candidate allTags then findAvailableTag (bumpTag candidate) else candidate

tagCmp (Just tagA) (Just tagB) =
    let (_, a) = vendorTag tagA
        (_, b) = vendorTag tagB in
    case compare a b of
      EQ -> case (releaseTag tagA, releaseTag tagB) of
              (Just (_, a), Just (_, b)) -> compare a b
              (Nothing, Nothing) -> EQ
              (Nothing, _) -> LT
              (_, Nothing) -> GT
      x -> x
tagCmp Nothing Nothing = EQ
tagCmp Nothing _ = LT
tagCmp _ Nothing = GT

tagMax :: [Maybe VersionTag] -> Maybe VersionTag
tagMax tags = head (sortBy (flip tagCmp) tags)

bumpTag tag@(VersionTag {releaseTag = Just (relName, relBuild)}) = tag {releaseTag = Just (relName, relBuild + 1)}
bumpTag tag@(VersionTag {vendorTag = (name, build), releaseTag = Nothing}) = tag {vendorTag = (name, build + 1)}

-- If one of the version number candidates has the wrong release name
-- this function fixes it, ensuring that the new tag isn't trumped by
-- the old.
fixReleaseName release tag@(VersionTag {vendorTag = (vendorName, vendorBuild)}) =
    case (release, releaseTag tag) of
      (Just relName, Just (oldRelName, _)) | relName == oldRelName -> tag
      -- If the release name doesn't match we need to bump the vendor build
      (Just relName, _) -> tag {vendorTag = (vendorName, vendorBuild+1), releaseTag = Just (relName, 1)}
      -- Removing an existing release tag always increases the version
      (Nothing, Just _) -> tag {releaseTag = Nothing}
      (Nothing, Nothing) -> tag {vendorTag = (vendorName, vendorBuild+1)}

-- This is similer to fixReleaseName, but we don't require the result to
-- trump the argument.  This is applied to the source package version number,
-- which is not necessarily present in the dist.
setReleaseName release tag =
    case (release, releaseTag tag) of
      (Just relName, Just (oldRelName, _)) | relName == oldRelName -> tag
      (Just relName, Just _) -> tag {releaseTag = Just (relName, 1)}
      (Just relName, Nothing) -> tag {releaseTag = Just (relName, 1)}
      (Nothing, _) -> tag {releaseTag = Nothing}
      

-- Return a tag that is
--  1) slightly newer than the dist tag
--  2) at least as new as the sourceTag
--  3) has the appropriate releaseTag
{-
nextTag vendor release extra sourceTag distTag =
    (fixRelease release sourceTag) (fixRelease release distTag)
@(VendorTag {vendorTag = (sourceVendor, sourceBuild)}) distTag@(VendorTag {vendorTag = (distVendor,  distBuild)}) =
    case (release, releaseTag sourceTag, releaseTag distTag) of
      _ | vendor /= sourceVendor || vendor /= distVendor ->	-- This really shouldn't happen
            error $ "Vendor tags do not match: (" ++ vendor ++ ", " ++ sourceVendor ++ ", " ++ distVendor ++ ")"
      (Just relName, Just (sourceRel, sourceRelBuild), _) | relName /= sourceRel -> 
      (Nothing, _, _) | sourceBuild > distBuild -> 
       -> 
    where
      fixRelease Nothing tag@(VendorTag {vendorTag = (vendor, build), releaseTag = Just (relName, relBuild)}) = tag {vendorTag = (vendor, build+1), releaseTag = Nothing}
      fixRelease (Just name) tag@(VendorTag {vendorTag = (vendor, build), releaseTag = Nothing})

-- |Return a tag which is slightly newer than the argument.  This needs to work
-- even if the release changes.
bumpTag newVendor release extra (Just tag@(VendorTag {vendorTag = (oldVendor, vendorBuild)})) =
    case (release, releaseTag tag) of
      _ | newVendor /= oldVendor -> error $ "Illegal vendor change: " ++ oldVendor ++ " -> " ++ newVendor
      (Just newName, Just (oldName, oldBuild)) | oldName == newName -> tag {releaseTag = Just (oldName, oldBuild + 1)}
      -- It is odd for the release name to change, but we can just
      -- bump the vendor build number to be safe
      (Just newName, Just (oldName, oldBuild)) -> tag {vendorTag = (oldVendor, vendorBuild+1), releaseTag = Just (newName, 1)}
      -- It is also odd for a release to switch from non-development
      -- to development.  So be it.
      (Nothing, Just (oldName, oldBuild)) -> tag {vendorTag = (oldVendor, vendorBuild+1), releaseTag = Nothing }
      -- 
      (Just newName, Nothing) -> tag {vendorTag = (
      (Nothing, Just (relName, relBuild)) ->
          
    case release of
      Nothing -> tag { vendorTag = let (vendor, build) = vendorTag tag in (vendor, build + 1) }
      Just name -> tag { releaseTag = case releaseTag tag of 
                                        Nothing -> Just (name, 1)
                                        Just (name, build) -> Just (name, build + 1) }
bumpTag vendor release extra Nothing = newTag vendor release extra

-- |Return a tag which is slightly newer, keeping the the same release.
bumpTag' tag@(VersionTag {vendorTag = (vendor, build), releaseTag = release}) =
    case releaseTag of
      Nothing -> tag {vendorTag = (vendor, build+1)}
      Just (name, relBuild) -> tag {releaseTag = Just (name, relBuild + 1)}
-}

-- Create a tag which is one click newer.
newTag vendor Nothing extra = VersionTag { extraNumber = extra, vendorTag = (vendor, 1), releaseTag = Nothing }
newTag vendor (Just name) extra = VersionTag { extraNumber = extra, vendorTag = (vendor, 0), releaseTag = Just (name, 1) }

-- | Format a tag as a string.
showTag :: (String -> String) -> VersionTag -> String
showTag alias (VersionTag { extraNumber = extra
                          , vendorTag = (vendor, vendorBuildNumber)
                          , releaseTag = releaseInfo }) =
   maybe "" (("r" ++) . show) extra ++
         vendor ++ show vendorBuildNumber ++
         maybe "" (\ (relname, relbuild) -> "~" ++ alias relname ++ show relbuild) releaseInfo

-- | Append a vendor tag to a string containing the revision portion
-- of a debian version number.
appendTag :: (String -> String) -> DebianVersion -> Maybe VersionTag -> DebianVersion
appendTag _ ver Nothing = ver
appendTag alias ver (Just tag) =
    case revision ver of
      Nothing -> setRevision ver (Just ("0" ++ showTag alias tag))
      Just rev -> case matchRegex numericSuffixRE rev of
                    Just [_, ""] -> setRevision ver (Just (rev ++ "0" ++ showTag alias tag))
                    Just [_, _] -> setRevision ver (Just (rev ++ showTag alias tag))
                    _ -> error "internal error"
    where numericSuffixRE = mkRegex "^(.*[^0-9])?([0-9]*)$"

-- | Change the revision part of a Debian version number.  (This may
-- belong in Debian.Version.)
setRevision :: DebianVersion -> Maybe String -> DebianVersion
setRevision ver rev = buildDebianVersion (epoch ver) (version ver) rev

-- Compare the version seen in the source code changelog to a version
-- seen in the distribution.  Since the autobuilder adds tags to the
-- version number of the packages it builds, the distribution version
-- may have been built from the source version even if they differ.
-- Specifically, we must assume that if the version matches when we
-- strip off one or both sections of the tag on the distribution
-- version number.
compareSourceAndDist vendor s d =
    let (verS, tagS) = parseTag vendor s
        (verD, tagD) = parseTag vendor d in
    let venS = maybe Nothing (Just . vendorTag) tagS
        venD = maybe Nothing (Just . vendorTag) tagD
        relS = maybe Nothing releaseTag tagS
        relD = maybe Nothing releaseTag tagD in
    case () of
      _ | verS /= verD -> compare verS verD
        | isNothing venS && isJust venD -> EQ
        | isNothing relS && isJust relD -> EQ
        | True -> compare s d
