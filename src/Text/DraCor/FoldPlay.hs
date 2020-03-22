module Text.DraCor.FoldPlay
  ( foldPlayWithPredicate
  , foldPlayWithPredicateToNum
  , foldPlayWithPredicate'
  , allPresent
  , nonePresent
  , foldPlayWithWindow
  , normalizeWithScenesCount
  , absoluteFrequency
  , absoluteFrequency'
  , subsequencesOfSize
  , longerThanOne
  ) where

-- | This module defines generic higher order functions for analyzing
-- dramatic texts. They are basically folds on the scenes of a
-- play. The functions can be used with various other simple functions
-- for calculating specific metrics. So writing metrics is a
-- composition of folds and simple other functions.
--
-- The functions are also generic in the sense, that characters
-- etc. may be represented by integers or even 8-bit words instead of
-- strings. This may have a boost on performance.

import qualified Data.HashMap.Lazy as Map
import Data.Hashable (Hashable)
import Data.List


-- * Folding a play using a predicate

-- | This is a generic fold that operates on the scenes as a whole and
-- calculates metrics based on a predicate like 'cooccurrenceP',
-- 'concomitanceP' or 'dominanceP'. Depending on the bool casting
-- function, the initial value and the aggregation function it can be
-- used for calculating binary or numerical values.
--
-- Binary: @ foldPlayWithPredicate id True (&&) concomitanceP @ will
-- calculate the concomitance of characters of a play like defined by
-- Solomon Marcus, who had binary values in mind: Two characters are
-- either concomitant or not.
--
-- Numerical: @ foldPlayWithPredicate fromEnum 0 (+) concomitanceP @
-- will calculate a numerical concomitance value for two characters
-- (or any set of characters). This way concomitance can be
-- interpreted not binary, but as a conditional probability of two
-- characters (or a set of characters), provided that the value is
-- mapped to the interval of [0,1] by a normalization function. See
-- 'foldPlayWithPredicateToNum' for a conveniant function composition.
--
-- The last two arguments: The characters present on each scene must
-- be given as the last argument. The butlast argument, the list of
-- sets of characters, also determines the type of measure, that is
-- calculated. For set with the cardinal number of one can be used to
-- count the pure presence or absence of each character per scene. For
-- set with higher cardinality, combinations of characters can be
-- counted, like cooccurrence or concomitance.
foldPlayWithPredicate
  :: (Bool -> b)                                -- ^ bool casting function, e.g. 'fromEnum'
  -> b                                          -- ^ initial value
  -> (b -> b -> b)                              -- ^ aggregation function
  -> ([a] -> [a] -> Bool)                       -- ^ metrics predicate
  -> [[a]] -- ^ set of character sets for which to calculate the metric
  -> [[a]] -- ^ scenes with present characters
  -> [([a], b)]
foldPlayWithPredicate boolCast initVal af p charSets scenes =
  foldl incInScene (zip charSets (repeat initVal)) scenes
  where
    incInScene acc scene = map (\(cs, v) -> (cs, (af v (boolCast $ p scene cs)))) acc

-- | Like 'foldPlayWithPredicate', but for calculating numerical
-- values, which are passed to a normalization function.
foldPlayWithPredicateToNum
  :: (Num i) =>
     ([a] -> [a] -> Bool)                       -- ^ metrics predicate
  -> ([[a]] -> [[a]] -> ([a], Int) -> ([a], i)) -- ^ normalization function
  -> [[a]] -- ^ set of character sets for which to calculate the metric
  -> [[a]] -- ^ scenes with present characters
  -> [([a], i)]
foldPlayWithPredicateToNum p nf charSets scenes =
  map (nf charSets scenes) $
  foldPlayWithPredicate fromEnum 0 (+) p charSets scenes


-- | Same as 'foldPlayWithToNum', but based on hashmap, which is not
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

-- | A predicate for reuse: True if all (and more) from the set are
-- present in the scene.
allPresent :: (Eq a) => [a] -> [a] -> Bool
allPresent scene set = foldl (\acc c -> acc && (c `elem` scene)) True set

-- | A predicate for reuse: True if none from the set is present in
-- the scene.
nonePresent :: (Eq a) => [a] -> [a] -> Bool
nonePresent scene set = foldl (\acc c -> acc && (not $ c `elem` scene)) True set


-- * Folding with a sliding window

-- | Fold a play by moving a window and calculating some measure
-- within it.
--
-- Example: Who answered how often on whom?
-- 
-- >>> foldPlayWithWindow 2 (const 1) (+) id [[1, 2, 3, 1, 2, 1, 2, 3, 1, 3, 1]]
-- [([2,1],3),([3,1],1),([1],1),([1,2],1),([1,3],3),([3,2],2)]
--
-- Read the result carefully: 2 answered 3 times on 1, 3 answered once
-- on 1, 1 once started a conversation, etc.
--
-- Example: Count how often characters take part in dialogue-like mini
-- structures, e.g. the sequence x y x is a dialogue-like mini
-- strukture for x.
--
-- >>> firstAndLast xs = if ((head xs) == (last xs)) then [head xs] else [-1]
-- >>> foldPlayWithWindow 3 (const 1) (+) firstAndLast [[1, 2, 3, 1, 2, 1, 2, 3, 1, 3, 1]]
-- [([1],3),([-1],6),([2],1),([3],1)]
--
-- Note that the count for character 1 is too high by 1. This results
-- from the beginning and the construction of the window there: For
-- the first step, the window size is still only 1 and character 1 is
-- the first and last character in this window. You can fix this by
-- checking the window size in the function for representing the
-- window. See 'atomicDialogues' for an example.
--
-- Please note that you can even pass in complete speach records and
-- then have the function for representing the window get the
-- character names from them and let the metrics value function
-- evaluate the speach records. So you may even calculate e.g. the
-- sentiment's slope within the window.
foldPlayWithWindow
  :: (Hashable b, Ord b, Num i) =>
     Int                 -- ^ window size
  -> ([a] -> i)          -- ^ metrics value function
  -> (i -> i -> i)       -- ^ metrics aggregation function
  -> ([a] -> [b])        -- ^ function for representing the window
  -> [[a]]               -- ^ scenes with speaches or speaking characters in order
  -> [([b], i)]
foldPlayWithWindow winSize mf af wf speaches =
  Map.toList $
  foldl1 (Map.unionWith af) $
  map (snd . (foldl updAggregation ([], Map.empty))) speaches
  where
    -- updAggregation :: ([a], Map.HashMap [b] i) -> a -> ([a], Map.HashMap [b] i)
    updAggregation (spoken, aggregation) current =
      (take (winSize - 1) window, Map.insertWith af (wf window) (mf window) aggregation)
      where
        window = (current:spoken)


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
