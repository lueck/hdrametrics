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


test_foldPlayWithPredicateNoCharsNoScenes = do
  assertEqual [] $ foldPlayWithPredicate allPresent normalizeWithScenesCount ([]::[[Int]]) ([]::[[Int]])

test_foldPlayWithPredicateNoScenes = do
  unitTestPending "division by zero when no scenes"
  assertEqual [([1,2], 0)] $ foldPlayWithPredicate allPresent normalizeWithScenesCount ([[1,2]]::[[Int]]) ([]::[[Int]])

test_foldPlayWithPredicateNoChars = do
  assertEqual [] $ foldPlayWithPredicate allPresent normalizeWithScenesCount ([]::[[Int]]) ([[1,2]]::[[Int]])


test_foldPlayWithPredicateAllPresent = do
  let foldPlay = foldPlayWithPredicate allPresent normalizeWithScenesCount :: Fractional i => ([[Int]] -> [[Int]] -> [([Int], i)])
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
