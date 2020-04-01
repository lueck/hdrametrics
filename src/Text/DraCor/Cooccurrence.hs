module Text.DraCor.Cooccurrence
  ( cooccurrenceP
  , cooccurrenceP'
  , cooccurrenceMapping
  ) where

-- | This module defines predicate functions for calculating the
-- cooccurrence metrics of two or more characters of a dramatic play.

import Data.List

import Text.DraCor.FoldPlay

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Map


-- | A predicate for calculating the cooccurence of two characters or
-- even a set of characters with an arbitrary cardinality number. The
-- cooccurrence of two characters is the \'classical\' approach to
-- graph-based drama analysis.
--
-- Usage:
-- @foldPlayWithPredicate id True (&&) cooccurrenceP characterSets scenes@
--
-- @foldPlayWithPredicateToNum normalizeWithScenesCount cooccurrenceP characterSets scenes@
cooccurrenceP
  :: (Eq a) =>
     [a]            -- ^ characters in scene
  -> [a]            -- ^ set of characters to calculate the metric for
  -> Bool
cooccurrenceP = allPresent

-- | Like 'cooccurrenceP', but implemented on set union.
cooccurrenceP' :: (Eq a) => [a] -> [a] -> Bool
cooccurrenceP' a b = (length (a `union` b) == (length a))


-- | 'cooccurrenceMapping' can be composed with 'foldPlayWithMapping'
-- for calculating the cooccurrences of characters of a play.
--
-- The function generates a 'Map.HashMap' from the combinations of the
-- characters of a scene.
--
-- Usage:
-- @cooccurence = foldPlayWithMapping (cooccurrenceMapping (map sort) 2) (+) absoluteFrequency@
--
-- The signature will be as follows:
-- @cooccurrence :: (Hashable a, Ord a) => [[a]] -> [[a]] -> [([a], Int)]@
--
-- You can now count cooccurrences:
-- >>> cooccurrence [] [[1,2], [1,3], [1,4]]
-- [([1,4],1),([1,2],1),([1,3],1)]
--
-- The character sets passed as first argument will generate zero
-- values for missing combinations:
-- >>> cooccurrence [[1,2], [1,5]] [[1,2], [1,3], [1,4]]
-- [([1,5],0),([1,4],1),([1,2],1),([1,3],1)]
--
-- The sorting argument has the following effect:
-- >>> cooccurrence [] [[2,1], [1,3], [4,1]]
-- [([1,4],1),([1,2],1),([1,3],1)]
--
-- This way we catch permutated sets:
-- >>> cooccurrence [] [[1,2], [1,3], [4,1], [2,1]]
-- [([1,4],1),([1,2],2),([1,3],1)]
--
-- With 'id' instead of the sorting, this would not be possible:
-- >>> cooccurrence' = foldPlayWithMapping (cooccurrenceMapping id 2) (+) absoluteFrequency
-- >>> cooccurrence' [] [[1,2], [1,3], [4,1], [2,1]]
-- [([2,1],1),([4,1],1),([1,2],1),([1,3],1)]
--
-- It is also possible to replace the sorting with permutations:
-- >>> cooccurrence' = foldPlayWithMapping (cooccurrenceMapping (concat . (map permutations)) 2) (+) absoluteFrequency
--
-- >>> cooccurrence' [] [[2,1], [1,3], [4,1]]
-- [([2,1],2),([3,1],1),([4,1],1),([1,4],1),([1,2],2),([1,3],1)]
cooccurrenceMapping
  :: (Hashable a, Ord a) =>
     ([[a]] -> [[a]])           -- ^ sorting function, e.g. @map sort@
  -> Int                        -- ^ cardinality number of the sets in the generated power set
  -> [a]                        -- ^ characters in the scene
  -> Map.HashMap [a] Int
cooccurrenceMapping sortFun cardinality chars =
  Map.fromList $!
  flip zip (repeat 1) $!
  sortFun $!
  filter longerThanOne $!
  subsequencesOfSize cardinality chars
