{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Text.DraCor.Dominance where

import Test.Framework

import Text.DraCor.Dominance

test_dominanceP = do
  --assertRaises (dominanceP ([]::[Int]) ([]::[Int]))
  --
  --assertRaises (dominanceP [1] ([]::[Int])) -- empty set
  assertBool $ dominanceP ([]::[Int]) [1] -- none present
  assertBool $ dominanceP ([]::[Int]) [1,2] -- none present
  --
  assertBool $ dominanceP [1] [1] -- all present
  -- minimal real world: a pair of characters
  assertBool $ dominanceP [3] [1,2] -- none present
  assertBool $ dominanceP [1,2] [1,2] -- all present
  assertBool $ dominanceP [1,2,3] [1,2] -- all and more present
  assertBool $ dominanceP [3,4,5] [1,2] -- none present
  assertBool $ dominanceP [1,3] [1,2] -- not all present
  assertBool $ dominanceP [1,2] [1,2,3] -- not all present
  assertBool $ not $ dominanceP [2,3] [1,2,3] -- first not present
  assertBool $ not $ dominanceP [2,4,5] [1,2,3] -- first not present

prop_dominanceP :: [Int] -> [Int] -> Bool
prop_dominanceP a b =
  if (length b < 1) then discard else
    (dominanceP a b == dominanceP' a b &&
     dominanceP a b == dominanceP'' a b &&
     dominanceP' a b == dominanceP'' a b)
