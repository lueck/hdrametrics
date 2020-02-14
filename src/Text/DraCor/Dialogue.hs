module Text.DraCor.Dialogue where

-- | This module defines function for counting dialogue-like
-- structures from a play.

import Data.Hashable (Hashable)

import Text.DraCor.FoldPlay

-- | A function for counting the dialogue-like atomic structures each
-- character takes part in. E.g. the sequence x y x is a dialogue-like
-- atomic structure for the character x.
atomicDialogues :: (Hashable a, Ord a) =>
                   a           -- ^ a non-existing character to count non-dialogues on.
                -> [[a]]       -- ^ scenes with speaking characters in order
                -> [([a], Int)]
atomicDialogues other = foldPlayWithWindow 3 1 (+) firstAndLast
  where
    firstAndLast xs =
      if ((head xs) == (last xs) && (length xs == 3))
      then [head xs]
      else [other]
