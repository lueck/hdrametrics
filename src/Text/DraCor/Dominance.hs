module Text.DraCor.Dominance
  ( dominanceP
  , dominanceP'
  , dominanceP''
  ) where

-- | This module defines predicate functions for calculating dominance
-- metrics of one character over others characters of a dramatic play.

import Data.List

import Text.DraCor.FoldPlay

-- | A predicate for calculating the dominance measure of a character
-- d over an other character (or a set of characters). The character d
-- must be the head of the list which is passed in as the second
-- argument.
--
-- Note that the length of this list must be at least 1, otherwise
-- there will be a runtime exception.
--
-- Usage:
-- @foldPlayWithPredicate normalizeWithScenesCount dominanceP characterSets scenes@
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
