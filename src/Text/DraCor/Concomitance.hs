module Text.DraCor.Concomitance
  ( concomitanceP
  , concomitanceP'
  , concomitanceP''
  , dominanceP
  , dominanceP'
  , dominanceP''
  , cooccurrenceP
  , cooccurrenceP'
  ) where

-- | This module defines functions for calculating metrics of two
-- or more characters of a dramatic play.

import Data.List

-- * Concomitance of characters

-- | A predicate for calculating the concomitance measure of two or
-- more characters using 'foldPlayWithPredicate'.
--
-- Usage:
-- @foldPlayWithPredicate normalizeWithScenesCount concomitanceP characterSets scenes@
concomitanceP
  :: (Eq a) =>
     [a]            -- ^ characters in scene
  -> [a]            -- ^ set of characters to calculate the metric for
  -> Bool
concomitanceP scene set = allPresent scene set || nonePresent scene set


-- | Same as 'concomitanceP', but other implementation.
concomitanceP' :: (Eq a) => [a] -> [a] -> Bool
concomitanceP' speakers set = all (`elem` speakers) set || all (not . (`elem` speakers)) set

-- | Same as 'concomitanceP', but other implementation.
concomitanceP'' :: (Eq a) => [a] -> [a] -> Bool
concomitanceP'' a b = (length (a `union` b) == (length a)) ||
                      (length (a `intersect` b) == 0)


-- * Dominance of a character about others

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
  (head set) `elem` scene ||
  nonePresent scene set

-- | Same as 'dominanceP', but with more explicit conditions.
dominanceP' :: (Eq a) => [a] -> [a] -> Bool
dominanceP' scene set =
  ((head set) `elem` scene && nonePresent scene (tail set)) ||
  ((head set) `elem` scene && any (`elem` scene) (tail set)) ||
  allPresent scene set ||
  nonePresent scene set

-- | Same as 'dominanceP', but implemented with set unions and intersections.
dominanceP''  :: (Eq a) => [a] -> [a] -> Bool
dominanceP'' a b =
  (length (a `union` b) == (length a)) ||
  ((head b) `elem` a && any (`elem` a) (tail b)) ||
  (((head b) `elem` a) && (length (a `intersect` (tail b)) ==0)) ||
  (length (a `intersect` b) == 0)


-- * Cooccurrence of characters

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


-- * Helper functions

allPresent :: (Eq a) => [a] -> [a] -> Bool
allPresent scene set = foldl (\acc c -> acc && (c `elem` scene)) True set

nonePresent :: (Eq a) => [a] -> [a] -> Bool
nonePresent scene set = foldl (\acc c -> acc && (not $ c `elem` scene)) True set

