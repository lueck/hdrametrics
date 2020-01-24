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

import Text.DraCor.Concomitance

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
          <> metavar "FILENAME")))
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
         <> help "CSV output. Note that CSV output only works properly for pairs of characters. This is --cardinality 2. For higher cardinality the set of characters is truncated after the second character."))
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
run gOpts@(Opts _ _ cOpts@(Concomitance _ _ _ _ _)) =
  concomitanceLikePlay concomitanceP gOpts cOpts
run gOpts@(Opts _ _ cOpts@(Dominance _ _ _ _ _)) =
  concomitanceLikePlay dominanceP gOpts cOpts
run gOpts@(Opts _ _ cOpts@(Cooccurrence _ _ _ _ _)) =
  concomitanceLikePlay cooccurrenceP gOpts cOpts

scenes :: Opts -> IO [[T.Text]]
scenes gOpts@(Opts (DraCorAPI url corpus play) _ _) = do
  ply' <- getPlay (fetch url) corpus play
  case ply' of
    Nothing -> do
      fail "Fatal: Could not parse JSON data"
    Just ply -> do
      return $ map scnSpeakers $ plySegments ply
scenes (Opts (ThreadUsersCSV fName) _ _) = do
  c <- B.readFile fName
  let csvOpts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord '\t') }
  case (Csv.decodeWith csvOpts Csv.HasHeader c :: Either String (V.Vector (T.Text, T.Text))) of
    Left err -> do
      fail err
    Right speakers -> do
      return $ snd $ V.foldl
        (\(lastThread, acc@(xs:xss)) (newThread, speaker) ->
           (if newThread==lastThread
            then (newThread, (speaker:xs):xss)
            else (newThread, [speaker]:acc)))
        ("", [[]])
        speakers

-- concomitanceLikePlay :: Eq a => ([a] -> [a] -> Bool) -> Opts -> Command -> IO ()
concomitanceLikePlay predicate gOpts cOpts = do
  scns <- scenes gOpts
  let characters = nub $ concat scns
      -- characterSets = filter ((\l -> l>=2 && l<=(concMaxCardinalNum cOpts)) . length) $
      --   subsequences characters
      characterSets = filter longerThanOne $
        subsequencesOfSize (concMaxCardinalNum cOpts) characters
      -- mapping of strings to integers
      characterIntTuples = zip [1..] characters
      getInt c = fst $ head $ filter ((==c) . snd) characterIntTuples
      characterMap = IntMap.fromList $ characterIntTuples
      sceneInts = map (map getInt) scns
      intSets = filter longerThanOne $
        subsequencesOfSize (concMaxCardinalNum cOpts) [1..(length characters)]
      -- calculate
      concomitanceValues = foldPlayWith predicate intSets sceneInts
      out = sortBy (concSortedBy cOpts) $ -- sortOn (sortOrder $ concSortedBy cOpts) $
        filter ((\v -> v >= (concLowerBoundConc cOpts) &&
                       v <= (concUpperBoundConc cOpts)) . snd) $
        map ((mkAbsoluteValues (concAbsoluteValues cOpts) (fromIntegral $ length scns)) . -- mk absolute values
             (\(is, v) -> ((map (characterMap IntMap.!) is), v)))
        concomitanceValues
  hPutStrLn stderr $ "Found scenes: " ++ (show $ length scns)
  hPutStrLn stderr $ "Found characters: " ++ (show $ length characters)
  hPutStrLn stderr $ "Size of generated powerset of characters: " ++ (show $ length characterSets)
  output gOpts out
  where
    sortBy MetricOrder = sortOn snd
    sortBy CardinalityOrder = sortOn (length . fst)
    sortBy _ = id
    longerThanOne :: [a] -> Bool
    longerThanOne [] = False
    longerThanOne (_:[]) = False
    longerThanOne _ = True
    mkAbsoluteValues :: Fractional i => Bool -> i -> (([a], i) -> ([a], i))
    mkAbsoluteValues True l = (\(cs, v) -> (cs, v*l))
    mkAbsoluteValues _ _ = id

-- | Format the output
output :: (ToJSON a, Csv.ToField a, Show a,
           ToJSON i, Csv.ToField i, Show i, Fractional i) =>
          Opts -> [([a], i)] -> IO ()
output (Opts _ Raw _) vals = do
  mapM_ print vals
output (Opts _ JSON _) vals = do
  print (toJSON vals)
output (Opts _ CSV _) vals = do
  -- We can only encode list of characters with fixed length, so we
  -- take 2, only!
  B.putStr $
    Csv.encode $
    map (\(cs, v) -> (head cs, head $ tail cs, v)) vals
