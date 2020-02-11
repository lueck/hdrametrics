{-# LANGUAGE Strict #-}
module Main where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as B
import Data.List
import System.IO
import qualified Data.IntMap as IntMap

import Text.DraCor.Types
import Text.DraCor.API

import Text.DraCor.FoldPlay
import Text.DraCor.Concomitance

import Criterion.Main


corpus = "ger"
play = "alberti-im-suff"

depth = 10000
lowerBound = 0.0
upperBound = 1.0

data SortOrder = NoSort | MetricOrder | CardinalityOrder



fetch :: String -> IO B.ByteString
fetch path = simpleHttp $ "https://dracor.org/api" ++ path


main = do
  ply' <- getPlay (fetch ) corpus play
  case ply' of
    Nothing -> do
      hPutStrLn stderr "Could find play"
    Just ply -> do
      let scenes = map scnSpeakers $ plySegments ply
          characters = nub $ concat scenes
          characterSets = filter longerThanOne $
            subsequencesOfSize depth characters
          -- mapping of strings to integers
          characterIntTuples = zip [1..] characters
          getInt c = fst $ head $ filter ((==c) . snd) characterIntTuples
          characterMap = IntMap.fromList $ characterIntTuples
          characterInts = [1..(length characters)]
          sceneInts = map (map getInt) scenes
          intSets = filter longerThanOne $
            subsequencesOfSize depth characterInts
          
      hPutStrLn stderr $ "Found scenes: " ++ (show $ length scenes)
      hPutStrLn stderr $ "Found characters: " ++ (show $ length characters)
      hPutStrLn stderr $ "Size of generated powerset of characters: " ++ (show $ length characterSets)
      defaultMain
        [ bgroup "implementations of foldPlayWithPredicate"
          [
            bench "foldPlayWithPredicate" $
            whnf (length . (uncurry (foldPlayWithPredicate concomitanceP absoluteFrequency)))
            (characterSets, scenes)
          , bench "foldPlayWithPredicate'" $
            whnf (length . (uncurry (foldPlayWithPredicate' concomitanceP absoluteFrequency)))
            (characterSets, scenes)
          ]
        , bgroup "foldPlayWithPredicate on text or integers"
          [
            bench "text" $
            whnf (length . (uncurry (foldPlayWithPredicate concomitanceP absoluteFrequency)))
            (characterSets, scenes)
          , bench "integers" $
            whnf (length . (uncurry (foldPlayWithPredicate concomitanceP absoluteFrequency)))
            (intSets, sceneInts)
          ]
        , bgroup "implementations of the concomitance-predicate"
          [
            bench "concomitanceP" $
            whnf (length . (uncurry (foldPlayWithPredicate concomitanceP absoluteFrequency)))
            (intSets, sceneInts)
          , bench "concomitanceP'" $
            whnf (length . (uncurry (foldPlayWithPredicate concomitanceP' absoluteFrequency)))
            (intSets, sceneInts)
          , bench "concomitanceP''" $
            whnf (length . (uncurry (foldPlayWithPredicate concomitanceP'' absoluteFrequency)))
            (intSets, sceneInts)
            ]
        ]
  where
    longerThanOne :: [a] -> Bool
    longerThanOne [] = False
    longerThanOne (_:[]) = False
    longerThanOne _ = True
