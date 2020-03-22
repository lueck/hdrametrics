{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Text.DraCor.FoldPlay where

import Test.Framework
import Data.List

import Text.DraCor.FoldPlay

test_longerThanOne = do
  assertBool $ not $ longerThanOne []
  assertBool $ not $ longerThanOne [1]
  assertBool $ longerThanOne [1, 2]
  assertBool $ longerThanOne [1..100]

test_longerThanOneInfinite = do
  assertBool $ longerThanOne $ repeat 'a'

test_subsequencesOfSizeAll = do
  assertEqual [] $
    [[], [0], [1], [2], [0,1], [0,2], [1,2], [0,1,2]] \\ subsequencesOfSize 3 [0,1,2]

test_subsequencesOfSizeAll' = do
  assertEqual [] $
    subsequencesOfSize 3 [0,1,2] \\ [[], [0], [1], [2], [0,1], [0,2], [1,2], [0,1,2]]

test_subsequencesOfSizeN = do
  assertEqual [] $
    [[], [0], [1], [2], [3], [0,1], [0,2], [0,3], [1,2], [1,3], [2,3]] \\ subsequencesOfSize 2 [0,1,2,3]

test_subsequencesOfSizeN' = do
  assertEqual [] $
    subsequencesOfSize 2 [0,1,2,3] \\ [[], [0], [1], [2], [3], [0,1], [0,2], [0,3], [1,2], [1,3], [2,3]]

test_subsequencesOfSizeNegative = do
  assertEqual [] $
    [[], [0], [1], [2], [0,1], [0,2], [1,2], [0,1,2]] \\ subsequencesOfSize (-1) [0,1,2]

test_subsequencesOfSizeNegative' = do
  assertEqual [] $
    subsequencesOfSize (-1) [0,1,2] \\ [[], [0], [1], [2], [0,1], [0,2], [1,2], [0,1,2]]


prop_subsequencesOfSize :: [Int] -> Bool
prop_subsequencesOfSize xs =
  subsequencesOfSize ((length xs)^2) (take 10 xs) \\ (subsequences $ take 10 xs) == []
  
prop_subsequencesOfSize' :: [Int] -> Bool
prop_subsequencesOfSize' xs =
  (subsequences $ take 10 xs) \\ (subsequencesOfSize ((length xs)^2) (take 10 xs)) == []

prop_subsequencesOfSizeN (NonNegative l) (xs :: [Int]) =
  subsequencesOfSize l (take 10 xs) \\ (filter ((<=l) . length) $ subsequences $ take 10 xs) == []
  
prop_subsequencesOfSizeN' (NonNegative l) (xs :: [Int]) =
  (filter ((<=l) . length) $ subsequences $ take 10 xs) \\ (subsequencesOfSize l (take 10 xs)) == []


test_foldPlayWithPredicateToNumNoCharsNoScenes = do
  assertEqual [] $ foldPlayWithPredicateToNum allPresent normalizeWithScenesCount ([]::[[Int]]) ([]::[[Int]])

test_foldPlayWithPredicateToNumNoScenes = do
  unitTestPending "division by zero when no scenes"
  assertEqual [([1,2], 0)] $ foldPlayWithPredicateToNum allPresent normalizeWithScenesCount ([[1,2]]::[[Int]]) ([]::[[Int]])

test_foldPlayWithPredicateToNumNoChars = do
  assertEqual [] $ foldPlayWithPredicateToNum allPresent normalizeWithScenesCount ([]::[[Int]]) ([[1,2]]::[[Int]])


test_foldPlayWithPredicateToNumAllPresent = do
  let foldPlay = foldPlayWithPredicateToNum allPresent normalizeWithScenesCount :: Fractional i => ([[Int]] -> [[Int]] -> [([Int], i)])
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


-- * foldPlayWithWindow

-- test_foldPlayWithWindow_empty = do
--   assertRaises
--     "throw exception"
--     (Exception "Prelude.head: empty list")
--     (foldPlayWithWindow 1 (const 1) (+) id [])

test_foldPlayWithWindow_nestedEmpty = do
  assertEqual ([]::[([Int], Int)]) $
    foldPlayWithWindow 1 (const 1) (+) id [[]]

test_foldPlayWithWindow_nestedEmpty' = do
  assertEqual ([]::[([Int], Int)]) $
    foldPlayWithWindow 5 (const 1) (+) id [[]]

test_foldPlayWithWindow_count = do
  assertEqual ([([1],7),([4],2),([2],3),([3],4)]::[([Int], Int)]) $
    foldPlayWithWindow 1 (const 1) (+) id [[1, 2, 3, 1, 2, 1, 2, 3, 1, 3, 1], [1, 3, 4, 1, 4]]

test_foldPlayWithWindow_answers = do
  assertEqual ([([2,1],3),([3,1],1),([1],1),([1,2],1),([1,3],3),([3,2],2)]::[([Int], Int)]) $
    foldPlayWithWindow 2 (const 1) (+) id [[1, 2, 3, 1, 2, 1, 2, 3, 1, 3, 1]]

test_foldPlayWithWindow_answers' = do
  assertEqual ([([4,3],1),([2,1],3),([3,1],2),([1],2),([4,1],1)
               ,([1,4],1),([1,2],1),([1,3],3),([3,2],2)]::[([Int], Int)]) $
    foldPlayWithWindow 2 (const 1) (+) id [[1, 2, 3, 1, 2, 1, 2, 3, 1, 3, 1], [1, 3, 4, 1, 4]]

test_foldPlayWithWindow_dialogues = do
  let firstAndLast xs = if ((head xs) == (last xs)) then [head xs] else [-1]
  assertEqual ([([1],3),([-1],6),([2],1),([3],1)]::[([Int], Int)]) $
    foldPlayWithWindow 3 (const 1) (+) firstAndLast [[1, 2, 3, 1, 2, 1, 2, 3, 1, 3, 1]]

test_foldPlayWithWindow_dialogues' = do
  let firstAndLast xs = if ((head xs) == (last xs) && (length xs == 3))
        then [head xs]
        else [-1]
  assertEqual ([([1],2),([-1],7),([2],1),([3],1)]::[([Int], Int)]) $
    foldPlayWithWindow 3 (const 1) (+) firstAndLast [[1, 2, 3, 1, 2, 1, 2, 3, 1, 3, 1]]
