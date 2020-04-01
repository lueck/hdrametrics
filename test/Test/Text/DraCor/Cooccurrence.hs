{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Text.DraCor.Cooccurrence where

import Test.Framework
import Data.List
import qualified Data.HashMap.Strict as Map

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


test_cooccurrenceMapping = do
  assertEqual 0 $ length
    (Map.difference
      (Map.fromList [([1,2],1), ([1,3],1), ([2,3],1)])
      (cooccurrenceMapping (map sort) 2 ([1,2,3]::[Int]))
    )
  assertEqual 0 $ length
    (Map.difference
      (cooccurrenceMapping (map sort) 2 ([1,2,3]::[Int]))
      (Map.fromList [([1,2],1), ([1,3],1), ([2,3],1)])
    )

prop_cooccurrenceMappingLength :: [Int] -> Bool
prop_cooccurrenceMappingLength chars =
  (Map.size $ cooccurrenceMapping (map sort) 2 chars') ==
  (length $ filter longerThanOne $ subsequencesOfSize 2 chars')
  where
    -- Should nub be moved to cooccurrenceMapping? No, because that
    -- would eleminate combinations of a character with itself. For
    -- plays, this cyclic feature would not be a disadvantage, but for
    -- folding over sentences it would. See
    -- test_cooccurrenceMappingDuplicates.
    chars' = nub chars

test_cooccurrenceMappingDuplicates = do
  assertEqual 0 $ length
    (Map.difference
      (Map.fromList [([1,2],1), ([1,3],1), ([2,3],1), ([1,1],1)])
      (cooccurrenceMapping (map sort) 2 ([1,2,3,1]::[Int]))
    )
  assertEqual 0 $ length
    (Map.difference
      (cooccurrenceMapping (map sort) 2 ([1,2,3,1]::[Int]))
      (Map.fromList [([1,2],1), ([1,3],1), ([2,3],1), ([1,1],1)])
    )


-- corner case
test_cooccurrenceMappingEmpty = do
  assertEqual (Map.empty :: Map.HashMap [Int] Int) $ cooccurrenceMapping (map sort) 2 []
