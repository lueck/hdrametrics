module Text.DraCor.Cooccurrence
  ( cooccurrenceP
  , cooccurrenceP'
  ) where

-- | This module defines predicate functions for calculating the
-- cooccurrence metrics of two or more characters of a dramatic play.

import Data.List

import Text.DraCor.FoldPlay


-- | A predicate for calculating the cooccurence of two characters or
-- even a set of characters with an arbitrary cardinality number. The
-- cooccurrence of two characters is the \'classical\' approach to
-- graph-based drama analysis.
--
-- Usage:
-- @foldPlayWithPredicate normalizeWithScenesCount cooccurrenceP characterSets scenes@
cooccurrenceP
  :: (Eq a) =>
     [a]            -- ^ characters in scene
  -> [a]            -- ^ set of characters to calculate the metric for
  -> Bool
cooccurrenceP = allPresent

-- | Like 'cooccurrenceP', but implemented on set union.
cooccurrenceP' :: (Eq a) => [a] -> [a] -> Bool
cooccurrenceP' a b = (length (a `union` b) == (length a))
