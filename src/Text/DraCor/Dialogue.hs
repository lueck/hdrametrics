module Text.DraCor.Dialogue where

-- | This module defines functions for counting dialogue-like
-- structures from a play.

import Data.Hashable (Hashable)

import Text.DraCor.FoldPlay

-- | A function for counting the dialogue-like atomic structures each
-- character takes part in. The sequence x y x is a dialogue-like
-- atomic structure for the character x. Note, that this is a formal
-- definition without respect to the content or even length of the
-- speaches.
atomicDialogues :: (Hashable a, Ord a) =>
                   a           -- ^ a non-existing character to count non-dialogues on.
                -> [[a]]       -- ^ scenes with speaking characters in order
                -> [([a], Int)]
atomicDialogues other = foldPlayWithWindow 3 (const 1) (+) firstAndLast
  where
    firstAndLast xs =
      if ((head xs) == (last xs) && (length xs == 3))
      then [head xs]
      else [other]
