module Text.DraCor.Concomitance
  ( foldPlayWith
  , concomitanceP
  ) where

-- | This module defines functions for calculating the concomitance
-- measure of two or more characters of a dramatic play.

import qualified Data.Map as Map
import Data.List

-- | A predicate for calculating the concomitance measure of two or
-- more characters using 'foldPlayWithCharacterSets'.
--
-- Usage: @foldPlayWithCharacterSets concomitanceP characterSets scenes@
concomitanceP
  :: (Eq a) =>
     [a]            -- ^ characters in scene
  -> [a]            -- ^ set of characters to calculate the metric for
  -> Bool
concomitanceP a b = (length (a `union` b) == (length a)) ||
                    (length (a `intersect` b) == 0)

-- | Calculate the metrics based on a given predicate. This is
-- essentially a fold on the scenes of the play.
foldPlayWith
  :: (Ord a, Fractional i) =>
     ([a] -> [a] -> Bool) -- ^ metrics predicate
  -> [[a]] -- ^ set of character sets for which to calculate the metric
  -> [[a]] -- ^ scenes with present characters
  -> [([a], i)]
foldPlayWith p charSet scenes =
  Map.toList $ -- return a list of tuples
  Map.map (\v -> (fromIntegral v) / (fromIntegral $ length scenes)) $ -- devide by count of scenes
  foldl (mapSceneWith p) -- fold using mapSceneWith as accumulator function
  (Map.fromList $ map (\k -> (k, 0)) charSet) -- init fold with Map of zeros
  scenes -- fold over scenes

-- | Map an incrementor function based on a predicate over the
-- character sets, do this for a specific scene.
mapSceneWith
  :: ([a] -> [a] -> Bool)       -- ^ predicate for the metric
  -> Map.Map [a] Int            -- ^ accumulator
  -> [a]                        -- ^ characters in new scene
  -> Map.Map [a] Int
mapSceneWith p acc scene = Map.mapWithKey (\k v -> v + (fromEnum $ p scene k)) acc
