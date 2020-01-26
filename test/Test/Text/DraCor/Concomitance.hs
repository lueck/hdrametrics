{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Text.DraCor.Concomitance where

import Test.Framework
import Data.List

import Text.DraCor.Concomitance

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


test_foldPlayWithNoCharsNoScenes = do
  assertEqual [] $ foldPlayWith concomitanceP ([]::[[Int]]) ([]::[[Int]])

test_foldPlayWithNoScenes = do
  unitTestPending "division by zero when no scenes"
  assertEqual [([1,2], 0)] $ foldPlayWith concomitanceP ([[1,2]]::[[Int]]) ([]::[[Int]])

test_foldPlayWithNoChars = do
  assertEqual [] $ foldPlayWith concomitanceP  ([]::[[Int]]) ([[1,2]]::[[Int]])

test_foldPlayWithConcomitance = do
  assertEqual [([1,2], 1)] $
    foldPlayWith concomitanceP ([[1,2]]::[[Int]]) ([[1,2]]::[[Int]])
  assertEqual [([1,2], 0.5)] $
    foldPlayWith concomitanceP ([[1,2]]::[[Int]]) ([[1,2], [1]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 1)/(fromIntegral 3)))] $
    foldPlayWith concomitanceP ([[1,2]]::[[Int]]) ([[1,2], [1], [2]]::[[Int]])
  assertEqual [([1,2], 0)] $
    foldPlayWith concomitanceP ([[1,2]]::[[Int]]) ([[1], [2]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 2)/(fromIntegral 4)))] $
    foldPlayWith concomitanceP ([[1,2]]::[[Int]]) ([[1,2], [1], [2], [3]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 2)/(fromIntegral 4)))] $
    foldPlayWith concomitanceP ([[1,2]]::[[Int]]) ([[4], [1], [2], [3]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 2)/(fromIntegral 4)))] $
    foldPlayWith concomitanceP ([[1,2]]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])
  assertEqual [([1,2,4], ((fromIntegral 1)/(fromIntegral 4)))] $
    foldPlayWith concomitanceP ([[1,2,4]]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])
  assertEqual [([1,2,3], ((fromIntegral 0)/(fromIntegral 4)))] $
    foldPlayWith concomitanceP ([[1,2,3]]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])
  assertEqual [([1,2,4], ((fromIntegral 1)/(fromIntegral 4)))
              ,([1,2,3], ((fromIntegral 0)/(fromIntegral 4)))
              ] $
    foldPlayWith concomitanceP ([[1,2,4]
                                ,[1,2,3]
                                ]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])


test_foldPlayWithCooccurrence = do
  assertEqual [([1,2], 1)] $
    foldPlayWith cooccurrenceP ([[1,2]]::[[Int]]) ([[1,2]]::[[Int]])
  assertEqual [([1,2], 0.5)] $
    foldPlayWith cooccurrenceP ([[1,2]]::[[Int]]) ([[1,2], [1]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 1)/(fromIntegral 3)))] $
    foldPlayWith cooccurrenceP ([[1,2]]::[[Int]]) ([[1,2], [1], [2]]::[[Int]])
  assertEqual [([1,2], 0)] $
    foldPlayWith cooccurrenceP ([[1,2]]::[[Int]]) ([[1], [2]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 1)/(fromIntegral 4)))] $
    foldPlayWith cooccurrenceP ([[1,2]]::[[Int]]) ([[1,2], [1], [2], [3]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 0)/(fromIntegral 4)))] $
    foldPlayWith cooccurrenceP ([[1,2]]::[[Int]]) ([[4], [1], [2], [3]]::[[Int]])
  assertEqual [([1,2], ((fromIntegral 1)/(fromIntegral 4)))] $
    foldPlayWith cooccurrenceP ([[1,2]]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])
  assertEqual [([1,2,4], ((fromIntegral 1)/(fromIntegral 4)))] $
    foldPlayWith cooccurrenceP ([[1,2,4]]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])
  assertEqual [([1,2,3], ((fromIntegral 0)/(fromIntegral 4)))] $
    foldPlayWith cooccurrenceP ([[1,2,3]]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])
  assertEqual [([1,2,4], ((fromIntegral 1)/(fromIntegral 4)))
              ,([1,2,3], ((fromIntegral 0)/(fromIntegral 4)))
              ] $
    foldPlayWith cooccurrenceP ([[1,2,4]
                                ,[1,2,3]
                                ]::[[Int]]) ([[1,2,4], [1,3], [2,4], [3,4]]::[[Int]])


prop_concomitanceP :: [Int] -> [Int] -> Bool
prop_concomitanceP a b =
  concomitanceP a b == concomitanceP' a b &&
  concomitanceP a b == concomitanceP'' a b &&
  concomitanceP' a b == concomitanceP'' a b

prop_dominanceP :: [Int] -> [Int] -> Bool
prop_dominanceP a b =
  if (length b < 1) then discard else
    dominanceP a b == dominanceP' a b

prop_cooccurrenceP :: [Int] -> [Int] -> Bool
prop_cooccurrenceP a b =
  cooccurrenceP a b == cooccurrenceP' a b
