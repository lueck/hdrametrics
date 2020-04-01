{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Control.Concurrent
import Data.List
import System.IO
import Data.Aeson
import qualified Data.IntMap as IntMap
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Data.Char

import Text.DraCor.Types
import Text.DraCor.API

import Text.DraCor.FoldPlay
import Text.DraCor.Concomitance
import Text.DraCor.Cooccurrence
import Text.DraCor.Dominance

data Opts = Opts
  { source :: DataSource
  , outpt :: Output
  , comnd :: Command
  }

data DataSource
  = DraCorAPI
    { url :: String
    , corpus :: String
    , play :: String
    }
  | ThreadUsersCSV String
  | IntegerCsv
    { scenesCsv :: String
    , charsCsv :: String
    }

data Output = Raw | JSON | CSV

data Command = Concomitance
  { concMaxCardinalNum :: Int
  , concLowerBoundConc :: Float
  , concUpperBoundConc :: Float
  , concAbsoluteValues :: Bool
  , concSortedBy :: SortOrder
  }
  | Dominance
  { concMaxCardinalNum :: Int
  , concLowerBoundConc :: Float
  , concUpperBoundConc :: Float
  , concAbsoluteValues :: Bool
  , concSortedBy :: SortOrder
  }
  | Cooccurrence
  { concMaxCardinalNum :: Int
  , concLowerBoundConc :: Float
  , concUpperBoundConc :: Float
  , concAbsoluteValues :: Bool
  , concSortedBy :: SortOrder
  }
  
data SortOrder = NoSort | MetricOrder | CardinalityOrder


opts_ :: Parser Opts
opts_ = Opts
  <$> (dracor_
       <|>
       (ThreadUsersCSV <$> strOption
         (long "thread-users-csv"
          <> help "Get scenes from a CSV file."
          <> metavar "FILENAME"))
       <|>
       integerCsv_)
  <*> ((flag Raw Raw
        (long "raw"
         <> help "Raw haskell output"))
       <|>
       (flag' JSON
        (long "json"
         <> help "JSON output"))
       <|>
       (flag' CSV
        (long "csv"
         <> help "CSV output. Note that CSV output only works properly for pairs of characters. This is --cardinality 2. For higher cardinality the set of characters is truncated after the second character!"))
      )
  <*> subparser
  (command "concomitance"
   (info (helper <*> (concomitanceLike_ "concomitance" Concomitance))
     (header "hdrametrics concomitance -- calculate concomitance measures"
      <> progDesc "Calculate concomitance measures for each pair of characters of a play or even for sets of characters with a cardinality number bigger than 2."
      <> footer "A list of concomitant characters following the notion of Solomon Marcus can be generated with these options: \"-c 2 -l 1.0\" A list of complementary characters following his notion can be generated with these options: \"-c 2 -u 0.0\""))
   --
   <> command "dominance"
   (info (helper <*> (concomitanceLike_ "dominance" Dominance))
     (header "hdrametrics dominance -- calculate dominance measures"
      <> progDesc "Calculate dominance measures for each character over each other character or even over a set of characters of a play"
      <> footer "A list of dominant characters over other characters following Solomon Marcus can be generated with these options: \"-c 2 -l 1.0\""))
   --
   <> command "cooccurrence"
   (info (helper <*> (concomitanceLike_ "cooccurrence" Cooccurrence))
     (header "hdrametrics cooccurrence -- calculate cooccurence measures"
      <> progDesc "Calculate cooccurence measures for each pair of characters of a play or even for an set of characters with an arbitrary cardinality number. It is normalized by division by the count of scenes in the play."))
  )

dracor_ :: Parser DataSource
dracor_ = DraCorAPI
  <$> strOption (long "url"
                 <> metavar "URL"
                 <> help "Base URL of dracor API."
                 <> showDefault
                 <> value "https://dracor.org/api")
  <*> strOption (long "corpus"
                 <> short 'c'
                 <> help "The corpus the play is contained in."
                 <> metavar "CORPUS")
  <*> strOption (long "play"
                 <> short 'p'
                 <> help "The identifier of the play."
                 <> metavar "PLAY")

integerCsv_ :: Parser DataSource
integerCsv_ = IntegerCsv
  <$> strOption (long "scenes-int-csv"
                 <> help "An integer representation of the characters present in scenes given in a CSV file."
                 <> metavar "FILENAME")
  <*> strOption (long "characters-ints"
                 <> help "A list of integers representing the characters of a play given. The integers must be separated by whitespace."
                 <> metavar "FILENAME")

concomitanceLike_ :: String -> (Int -> Float -> Float -> Bool -> SortOrder -> Command) -> Parser Command
concomitanceLike_ helpStr constructor = constructor
  <$> option auto (long "maxcard"
                   <> short 'c'
                   <> help "Maximal cardinal number of a character set for which the concomitance is calculated. This must be an integer value. A value of 2 will calculate the concomitance for all pairs of characters. If this option is left, the concomitance is calculated for all sets in the power set of the set of characters, for which the cardinality equals or exceeds 2."
                   <> metavar "CARDINALITY"
                   <> value (maxBound :: Int))
  <*> option auto (long "lowerbound"
                   <> short 'l'
                   <> help "Lower bound threshold for the output. Character sets with a concomitance lower than this threshold will be removed from the output."
                   <> metavar "CONCOMITANCE"
                   <> value 0.0
                   <> showDefault)
  <*> option auto (long "upperbound"
                   <> short 'u'
                   <> help "Upper bound threshold for the output. Character sets with a concomitance bigger than this threshold will be removed from the output."
                   <> metavar "CONCOMITANCE"
                   <> value 1.0
                   <> showDefault)
  <*> switch (long "absolute"
              <> help "Output absolute frequency instead of normalized values. You will have to adjust the --upperbound option in combination with this.")
  <*> sortOrder_

sortOrder_ :: Parser SortOrder
sortOrder_ =
  (flag NoSort NoSort
   (long "nosort"
    <> help "Do not sort output, i.e. preserve order of occurence in the play. (default)"))
  <|>
  (flag' CardinalityOrder
   (long "cardinality"
    <> help "Sort output by the cardinality number of the set of characters."))
  <|>
  (flag' MetricOrder
   (long "metric"
    <> help "Sort output by the metric's value."))

main = execParser opts >>= run
  where opts = info (helper <*> opts_)
          (fullDesc
           <> progDesc
           "hdrametrics calculates metrics of a dramatic text."
           <> header
           "hdrametrics - calculate metrics of a dramatic text.")


fetch :: String -> String -> IO B.ByteString
-- fetch url path = do
--   request <- parseRequest $ url ++ path
--   manager <- newManager tlsManagerSettings
--   return $ responseBody $ httpLbs request manager
fetch url path = simpleHttp $ url ++ path


run :: Opts -> IO ()
run gOpts@(Opts _ _ cOpts@(Concomitance card _ _ _ _)) =
  concomitanceLikePlay
  (foldPlayWithPredicateToNum concomitanceP)
  ((filter longerThanOne) . (subsequencesOfSize card))
  gOpts cOpts
run gOpts@(Opts _ _ cOpts@(Dominance card _ _ _ _)) =
  concomitanceLikePlay
  (foldPlayWithPredicateToNum dominanceP)
  (concat . (map permutations) . (filter longerThanOne) . (subsequencesOfSize card))
  gOpts cOpts
run gOpts@(Opts _ _ cOpts@(Cooccurrence card _ _ _ _)) =
  concomitanceLikePlay
  --(foldPlayWithPredicateToNum cooccurrenceP)
  (foldPlayWithMapping (cooccurrenceMapping (concat . (map permutations)) 2) (+)) -- (map sort) 2) (+))
  ((filter longerThanOne) . (subsequencesOfSize card))
  gOpts cOpts

scenes :: Opts -> IO ([[Int]], [Int], (Int -> T.Text))
scenes gOpts@(Opts (DraCorAPI url corpus play) _ _) = do
  ply' <- getPlay (fetch url) corpus play
  case ply' of
    Nothing -> do
      fail "Fatal: Could not parse JSON data"
    Just ply -> do
      return $ mkIntRepresentation $ map scnSpeakers $ plySegments ply
scenes (Opts (ThreadUsersCSV fName) _ _) = do
  c <- B.readFile fName
  let csvOpts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ',') }
  case (Csv.decodeWith csvOpts Csv.HasHeader c :: Either String (V.Vector (T.Text, T.Text))) of
    Left err -> do
      fail err
    Right speakers -> do
      return $ mkIntRepresentation $
        snd $ V.foldl
        (\(lastThread, acc@(xs:xss)) (newThread, speaker) ->
           (if newThread==lastThread
            then (newThread, (speaker:xs):xss)
            else (newThread, [speaker]:acc)))
        ("", [[]])
        speakers
scenes (Opts (IntegerCsv scns chars) _ _) = do
  s <- readIntScenes scns
  charsH <- openFile chars ReadMode
  contents <- hGetContents charsH
  let nums = f (words contents)
  return (s, nums, (T.pack . show))
  where
    f :: [String] -> [Int]
    f = map read

readIntScenes :: String -> IO ([[Int]])
readIntScenes scns = do
  s <- B.readFile scns
  let csvOpts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ',') }
  case (Csv.decodeWith csvOpts Csv.HasHeader s :: Either String (V.Vector (Int, Int))) of
    Left err -> do
      fail err
    Right speakers -> do
      return $ snd $ V.foldl
        (\(lastThread, acc@(xs:xss)) (newThread, speaker) ->
           (if newThread==lastThread
            then (newThread, (speaker:xs):xss)
            else (newThread, [speaker]:acc)))
        ((-1), [[]])
        speakers

mkIntRepresentation :: [[T.Text]] -> ([[Int]], [Int], (Int -> T.Text))
mkIntRepresentation scns =
  ( (map (map getInt) scns)
  , [1..(length characters)]
  , (characterMap IntMap.!)
  )
  where
    characters :: [T.Text]
    characters = nub $ concat scns
    characterIntTuples :: [(Int, T.Text)]
    characterIntTuples = zip [1..] characters
    getInt :: T.Text -> Int
    getInt c = fst $ head $ filter ((==c) . snd) characterIntTuples
    characterMap :: IntMap.IntMap T.Text
    characterMap = IntMap.fromList $ characterIntTuples

-- concomitanceLikePlay :: Eq a => ([a] -> [a] -> Bool) -> Opts -> Command -> IO ()
concomitanceLikePlay foldFun mkPowerSet gOpts cOpts = do
  (scns, chars, int2Name) <- scenes gOpts
  let intSets = mkPowerSet chars
      -- calculate
      normFun = if (concAbsoluteValues cOpts)
        then absoluteFrequency'
        else normalizeWithScenesCount
      concomitanceValues = foldFun normFun intSets scns
      out = sortBy (concSortedBy cOpts) $ -- sortOn (sortOrder $ concSortedBy cOpts) $
        filter ((\v -> v >= (concLowerBoundConc cOpts) &&
                       v <= (concUpperBoundConc cOpts)) . snd) $
        map (\(is, v) -> ((map int2Name is), v))
        concomitanceValues
  hPutStrLn stderr $ "Found scenes: " ++ (show $ length scns)
  hPutStrLn stderr $ "Found characters: " ++ (show $ length chars)
  hPutStrLn stderr $ "Size of generated powerset of characters: " ++ (show $ length intSets)
  output gOpts out
  where
    sortBy MetricOrder = sortOn snd
    sortBy CardinalityOrder = sortOn (length . fst)
    sortBy _ = id

-- | Format the output
output :: (ToJSON a, Csv.ToField a, Show a,
           ToJSON i, Csv.ToField i, Show i) =>
          Opts -> [([a], i)] -> IO ()
output (Opts _ Raw _) vals = do
  mapM_ print vals
output (Opts _ JSON _) vals = do
  print (toJSON vals)
output (Opts _ CSV _) vals = do
  -- We can only encode character sets of fixed cardinality to CSV, so
  -- we take 2 (pairs), only!
  B.putStr $
    Csv.encode $
    map (\(cs, v) -> (head cs, head $ tail cs, v)) vals
