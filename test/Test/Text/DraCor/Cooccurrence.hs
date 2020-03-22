{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Text.DraCor.Cooccurrence where

import Test.Framework
import Data.List

import Text.DraCor.FoldPlay
import Text.DraCor.Cooccurrence

test_foldPlayWithPredicateCooccurrence = do
  let foldPlay = foldPlayWithPredicateToNum cooccurrenceP normalizeWithScenesCount :: Fractional i => ([[Int]] -> [[Int]] -> [([Int], i)])
  assertEqual [([1,2], 1)] $
    foldPlay ([[1,2]]::[[Int]]) ([[1,2]]::[[Int]])
  assertEqual [([1,2], 0.5)] $
    foldPlay ([[1,2]]::[[Int]]) ([[1,2], [1]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 1)/(fromIntegral 3)))] $
    foldPlay ([[1,2]]::[[Int]]) ([[1,2], [1], [2]]::[[Int]])
  assertEqual [([1,2], 0)] $
    foldPlay ([[1,2]]::[[Int]]) ([[1], [2]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 1)/(fromIntegral 4)))] $
    foldPlay ([[1,2]]::[[Int]]) ([[1,2], [1], [2], [3]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 0)/(fromIntegral 4)))] $
    foldPlay ([[1,2]]::[[Int]]) ([[4], [1], [2], [3]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 1)/(fromIntegral 4)))] $
    foldPlay ([[1,2]]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])
  assertEqual [([1,2,4], ((fromIntegral 1)/(fromIntegral 4)))] $
    foldPlay ([[1,2,4]]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])
  assertEqual [([1,2,3], ((fromIntegral 0)/(fromIntegral 4)))] $
    foldPlay ([[1,2,3]]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])
  assertEqual [([1,2,4], ((fromIntegral 1)/(fromIntegral 4)))
              ,([1,2,3], ((fromIntegral 0)/(fromIntegral 4)))
              ] $
    foldPlay ([[1,2,4]
              ,[1,2,3]
              ]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])

test_cooccurrenceP = do
  assertBool $ cooccurrenceP ([]::[Int]) ([]::[Int])
  --
  assertBool $ cooccurrenceP [1] ([]::[Int]) -- empty set
  assertBool $ not $ cooccurrenceP ([]::[Int]) [1] -- none present
  assertBool $ not $ cooccurrenceP ([]::[Int]) [1,2] -- none present
  --
  assertBool $ cooccurrenceP [1] [1] -- all present
  -- -- minimal real world: a pair of characters
  assertBool $ not $ cooccurrenceP [3] [1,2] -- none present
  assertBool $ cooccurrenceP [1,2] [1,2] -- all present
  assertBool $ cooccurrenceP [1,2,3] [1,2] -- all and more present
  assertBool $ not $ cooccurrenceP [3,4,5] [1,2] -- none present
  assertBool $ not $ cooccurrenceP [1,3] [1,2] -- not all present
  assertBool $ not $ cooccurrenceP [1,2] [1,2,3] -- not all present

prop_cooccurrenceP :: [Int] -> [Int] -> Bool
prop_cooccurrenceP a b =
  cooccurrenceP a b == cooccurrenceP' a b
