module Text.DraCor.Concomitance
  ( foldPlayWith
  , foldPlayWith'
  , concomitanceP
  , concomitanceP'
  , concomitanceP''
  , dominanceP
  , dominanceP'
  , cooccurrenceP
  , cooccurrenceP'
  , subsequencesOfSize
  , longerThanOne
  ) where

-- | This module defines functions for calculating the concomitance
-- measure of two or more characters of a dramatic play.

import qualified Data.HashMap.Lazy as Map
import Data.Hashable (Hashable)
import Data.List

-- * Metrics

-- | A predicate for calculating the concomitance measure of two or
-- more characters using 'foldPlayWithCharacterSets'.
--
-- Usage: @foldPlayWithCharacterSets concomitanceP characterSets scenes@
concomitanceP
  :: (Eq a) =>
     [a]            -- ^ characters in scene
  -> [a]            -- ^ set of characters to calculate the metric for
  -> Bool
concomitanceP scene set = allPresent scene set || nonePresent scene set


allPresent :: (Eq a) => [a] -> [a] -> Bool
allPresent scene set = foldl (\acc c -> acc && (c `elem` scene)) True set

nonePresent :: (Eq a) => [a] -> [a] -> Bool
nonePresent scene set = foldl (\acc c -> acc && (not $ c `elem` scene)) True set

-- | Same as 'concomitanceP', but other implementation.
concomitanceP' :: (Eq a) => [a] -> [a] -> Bool
concomitanceP' speakers set = all (`elem` speakers) set || all (not . (`elem` speakers)) set

-- | Same as 'concomitanceP', but other implementation.
concomitanceP'' :: (Eq a) => [a] -> [a] -> Bool
concomitanceP'' a b = (length (a `union` b) == (length a)) ||
                      (length (a `intersect` b) == 0)

-- | A predicate for calculating the dominance measure of a character
-- d over an other character (or a set of characters). The character d
-- must be the head of the list which is passed in as the second
-- argument.
--
-- Note that the length of this list must be at least 1, otherwise
-- there will be a runtime exception.
dominanceP
  :: (Eq a) =>
     [a]            -- ^ characters in scene
  -> [a]            -- ^ set of characters to calculate the metric for
  -> Bool
dominanceP scene set =
  ((head set) `elem` scene && nonePresent scene (tail set)) ||
  ((head set) `elem` scene && any (`elem` scene) (tail set)) ||
  allPresent scene set ||
  nonePresent scene set

-- | Same as 'dominanceP', but implemented with set unions and intersections.
dominanceP'  :: (Eq a) => [a] -> [a] -> Bool
dominanceP' a b =
  (length (a `union` b) == (length a)) ||
  ((head b) `elem` a && any (`elem` a) (tail b)) ||
  (((head b) `elem` a) && (length (a `intersect` (tail b)) ==0)) ||
  (length (a `intersect` b) == 0)

-- | A predicate for calculating the cooccurence of two characters or
-- even a set of characters with an arbitrary cardinality number. The
-- cooccurrence of two characters is the \'classical\' approach to
-- graph-based drama analysis.
cooccurrenceP
  :: (Eq a) =>
     [a]            -- ^ characters in scene
  -> [a]            -- ^ set of characters to calculate the metric for
  -> Bool
cooccurrenceP = allPresent

-- | Like 'cooccurrenceP', but implemented on set union.
cooccurrenceP' :: (Eq a) => [a] -> [a] -> Bool
cooccurrenceP' a b = (length (a `union` b) == (length a))



-- * Folding a play

-- | Calculate the metrics based on a given predicate. This is
-- essentially a fold on the scenes of the play.
foldPlayWith
  :: (Fractional i) =>
     ([a] -> [a] -> Bool) -- ^ metrics predicate
  -> [[a]] -- ^ set of character sets for which to calculate the metric
  -> [[a]] -- ^ scenes with present characters
  -> [([a], i)]
foldPlayWith p charSets scenes =
  map (\(cs, v) -> (cs, (fromIntegral v)/(fromIntegral $ length scenes))) $
  foldl incInScene (map (\cs -> (cs,0)) charSets) scenes
  where
    incInScene acc scene = map (\(cs, v) -> (cs, (v + (fromEnum $ p scene cs)))) acc


-- | Same as 'foldPlayWith', but based on hashmap, which is not
-- needed, because we do not lookup anything, but instead map over the
-- whole hashmap. So this is not faster, but a magnitude slower.
foldPlayWith'
  :: (Hashable a, Ord a, Fractional i) =>
     ([a] -> [a] -> Bool) -- ^ metrics predicate
  -> [[a]] -- ^ set of character sets for which to calculate the metric
  -> [[a]] -- ^ scenes with present characters
  -> [([a], i)]
foldPlayWith' p charSet scenes =
  Map.toList $ -- return a list of tuples
  Map.map (\v -> (fromIntegral v) / (fromIntegral $ length scenes)) $ -- devide by count of scenes
  foldl (mapSceneWith' p) -- fold using mapSceneWith as accumulator function
  (Map.fromList $ map (\k -> (k, 0)) charSet) -- init fold with Map of zeros
  scenes -- fold over scenes

mapSceneWith'
  :: ([a] -> [a] -> Bool)       -- ^ predicate for the metric
  -> Map.HashMap [a] Int            -- ^ accumulator
  -> [a]                        -- ^ characters in new scene
  -> Map.HashMap [a] Int
mapSceneWith' p acc scene = Map.mapWithKey (\k v -> v + (fromEnum $ p scene k)) acc



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
