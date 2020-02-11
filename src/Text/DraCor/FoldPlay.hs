module Text.DraCor.FoldPlay
  ( foldPlayWithPredicate
  , foldPlayWithPredicate'
  , normalizeWithScenesCount
  , absoluteFrequency
  , absoluteFrequency'
  , subsequencesOfSize
  , longerThanOne
  ) where

import qualified Data.HashMap.Lazy as Map
import Data.Hashable (Hashable)
import Data.List


-- * Folding a play using a predicate

-- | Calculate the metrics based on a given predicate like
-- 'concomitanceP'. This is essentially a fold on the scenes of the
-- play.
foldPlayWithPredicate
  :: (Num i) =>
     ([a] -> [a] -> Bool)                       -- ^ metrics predicate
  -> ([[a]] -> [[a]] -> ([a], Int) -> ([a], i)) -- ^ normalization function
  -> [[a]] -- ^ set of character sets for which to calculate the metric
  -> [[a]] -- ^ scenes with present characters
  -> [([a], i)]
foldPlayWithPredicate p normFun charSets scenes =
  map (normFun charSets scenes) $
  foldl incInScene (zip charSets (repeat 0)) scenes
  where
    incInScene acc scene = map (\(cs, v) -> (cs, (v + (fromEnum $ p scene cs)))) acc


-- | Same as 'foldPlayWith', but based on hashmap, which is not
-- needed, because we do not lookup anything, but instead map over the
-- whole hashmap. So this is not faster, but a magnitude slower.
foldPlayWithPredicate'
  :: (Hashable a, Ord a, Num i) =>
     ([a] -> [a] -> Bool) -- ^ metrics predicate
  -> ([[a]] -> [[a]] -> ([a], Int) -> ([a], i)) -- ^ normalization function
  -> [[a]] -- ^ set of character sets for which to calculate the metric
  -> [[a]] -- ^ scenes with present characters
  -> [([a], i)]
foldPlayWithPredicate' p normFun charSets scenes =
  Map.toList $ -- return a list of tuples
  Map.mapWithKey (curry (snd . (normFun charSets scenes))) $ -- devide by count of scenes or so
  foldl (mapSceneWithPredicate' p) -- fold using mapSceneWith as accumulator function
  (Map.fromList $ zip charSets (repeat 0)) -- init fold with Map of zeros
  scenes -- fold over scenes

mapSceneWithPredicate'
  :: ([a] -> [a] -> Bool)       -- ^ predicate for the metric
  -> Map.HashMap [a] Int        -- ^ accumulator
  -> [a]                        -- ^ characters in new scene
  -> Map.HashMap [a] Int
mapSceneWithPredicate' p acc scene = Map.mapWithKey (\k v -> v + (fromEnum $ p scene k)) acc


-- * Normalization functions

-- | Divide the value by the count of scenes.
normalizeWithScenesCount
  :: (Fractional i) =>
     [[a]] -- ^ set of character sets for which to calculate the metric
  -> [[a]] -- ^ scenes with present characters
  -> ([a], Int)                 -- ^ absolute values
  -> ([a], i)
normalizeWithScenesCount _ scenes (cs, val) =
  (cs, (fromIntegral val) / (fromIntegral $ length scenes))

-- | Do not normalize, use absolute values instead.
absoluteFrequency
  :: [[a]] -- ^ set of character sets for which to calculate the metric
  -> [[a]] -- ^ scenes with present characters
  -> ([a], Int)                 -- ^ absolute values
  -> ([a], Int)
absoluteFrequency _ _ = id

-- | Like absoluteFrequency, but the result is a 'Fractional', instead
-- of an 'Int' value.
absoluteFrequency'
  :: (Fractional i) =>
     [[a]] -- ^ set of character sets for which to calculate the metric
  -> [[a]] -- ^ scenes with present characters
  -> ([a], Int)                 -- ^ absolute values
  -> ([a], i)
absoluteFrequency' _ _ (cs, v) = (cs, fromIntegral v)


-- * Helper function for generating powersets represented with lists

-- | Return the list of all subsequences of the list given as second
-- argument, with length lower or equal the first argument.
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize 0 _ = [[]]
subsequencesOfSize _ [] = [[]]
subsequencesOfSize i (x:xs) =
  (map (x:) $ subsequencesOfSize (i-1) xs) ++ (subsequencesOfSize i xs)
  -- FIXME: get rid of ++

-- | A predicate that is true when and only when the length of the
-- sequence given as parameter exceeds 1.
longerThanOne :: [a] -> Bool
longerThanOne [] = False
longerThanOne (_:[]) = False
longerThanOne _ = True