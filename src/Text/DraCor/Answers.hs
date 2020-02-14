module Text.DraCor.Answers
  ( answers
  ) where

-- | This module provides functions for counting how often a character
-- answered to an other character or started a scene (a
-- conversation). Note, that this measure is directed.

import Data.Hashable (Hashable)

import Text.DraCor.FoldPlay

-- | Count, how often each character answered to each other. For pairs
-- of characters, that are missing in the result, there is no answer;
-- zero values are not in the result.
answers :: (Hashable a, Ord a) =>
           [[a]]         -- ^ scenes with speaking characters in order
        -> [([a], Int)]
answers = foldPlayWithWindow 2 1 (+) id
