module Text.DraCor.Concomitance
  ( concomitanceP
  , concomitanceP'
  , concomitanceP''
  ) where

-- | This module defines functions for calculating the concomitance
-- metrics of two or more characters of a dramatic play.

import Data.List

import Text.DraCor.FoldPlay

-- | A predicate for calculating the concomitance measure of two or
-- more characters using 'foldPlayWithPredicate'.
--
-- Usage:
-- @foldPlayWithPredicate id True (&&) concomitanceP characterSets scenes@
--
-- @foldPlayWithPredicateToNum normalizeWithScenesCount concomitanceP characterSets scenes@

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
