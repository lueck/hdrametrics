{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Text.DraCor.Concomitance where

import Test.Framework

import Text.DraCor.FoldPlay
import Text.DraCor.Concomitance

test_foldPlayWithPredicateConcomitance = do
  let foldPlay = foldPlayWithPredicate concomitanceP normalizeWithScenesCount :: Fractional i => ([[Int]] -> [[Int]] -> [([Int], i)])
  assertEqual [([1,2], 1)] $
    foldPlay ([[1,2]]::[[Int]]) ([[1,2]]::[[Int]])
  assertEqual [([1,2], 0.5)] $
    foldPlay ([[1,2]]::[[Int]]) ([[1,2], [1]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 1)/(fromIntegral 3)))] $
    foldPlay ([[1,2]]::[[Int]]) ([[1,2], [1], [2]]::[[Int]])
  assertEqual [([1,2], 0)] $
    foldPlay ([[1,2]]::[[Int]]) ([[1], [2]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 2)/(fromIntegral 4)))] $
    foldPlay ([[1,2]]::[[Int]]) ([[1,2], [1], [2], [3]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 2)/(fromIntegral 4)))] $
    foldPlay ([[1,2]]::[[Int]]) ([[4], [1], [2], [3]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 2)/(fromIntegral 4)))] $
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

test_concomitanceP = do
  assertBool $ concomitanceP ([]::[Int]) ([]::[Int])
  --
  assertBool $ concomitanceP [1] ([]::[Int]) -- empty set
  assertBool $ concomitanceP ([]::[Int]) [1] -- none present
  assertBool $ concomitanceP ([]::[Int]) [1,2] -- none present
  --
  assertBool $ concomitanceP [1] [1] -- all present
  -- -- minimal real world: a pair of characters
  assertBool $ concomitanceP [3] [1,2] -- none present
  assertBool $ concomitanceP [1,2] [1,2] -- all present
  assertBool $ concomitanceP [1,2,3] [1,2] -- all and more present
  assertBool $ concomitanceP [3,4,5] [1,2] -- none present
  assertBool $ not $ concomitanceP [1,3] [1,2] -- not all present
  assertBool $ not $ concomitanceP [1,2] [1,2,3] -- not all present

prop_concomitanceP :: [Int] -> [Int] -> Bool
prop_concomitanceP a b =
  concomitanceP a b == concomitanceP' a b &&
  concomitanceP a b == concomitanceP'' a b &&
  concomitanceP' a b == concomitanceP'' a b
