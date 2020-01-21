module Main where

import Options.Applicative
import Network.HTTP.Conduit
import Conduit (runConduit)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Control.Concurrent
import Data.List
import System.IO
import Data.Aeson
import qualified Data.Csv as Csv

import Text.DraCor.Types
import Text.DraCor.API

import Text.DraCor.Concomitance

data Opts = Opts
  { url :: String
  , corpus :: String
  , play :: String
  , outpt :: Output
  , comnd :: Command
  }

data Output = Raw | JSON --  | CSV

data Command = Concomitance
  { concMaxCardinalNum :: Int
  , concLowerBoundConc :: Float
  , concUpperBoundConc :: Float
  , concSortedBy :: SortOrder
  }
  
data SortOrder = NoSort | MetricOrder | CardinalityOrder


opts_ :: Parser Opts
opts_ = Opts
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
  <*> ((flag Raw Raw
        (long "raw"
         <> help "Raw haskell output"))
       <|>
       (flag' JSON
        (long "json"
         <> help "JSON output"))
       -- <|>
       -- (flag' CSV
       --  (long "csv"
       --   <> help "CSV output"))
      )
  <*> subparser
  (command "concomitance"
   (info (helper <*> concomitance_)
     (header "hdrametrics concomitance -- calculate concomitance measures"
      <> progDesc "Calculate concomitance measures for each pair of characters of a play or even for sets of characters with a cardinality number bigger than 2."
      <> footer "A list of concomitant characters following the notion of Solomon Marcus can be generated with these options: \"-c 2 -l 1.0\" A list of complementary characters following his notion can be generated with these options: \"-c 2 -u 0.0\"")))

concomitance_ :: Parser Command
concomitance_ = Concomitance
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
           "drametrics calculates a metric for a dramatic text."
           <> header
           "drametrics - calculate metrics for a dramatic text.")


fetch' :: String -> String -> IO B.ByteString
-- fetch url path = do
--   request <- parseRequest $ url ++ path
--   manager <- newManager tlsManagerSettings
--   return $ responseBody $ httpLbs request manager
fetch' url path = simpleHttp $ url ++ path

fetch :: Opts -> (String -> IO B.ByteString)
fetch opts = fetch' $ url opts

run :: Opts -> IO ()
run gOpts@(Opts _ _ _ _ cOpts@(Concomitance _ _ _ _)) = concomitancePlay gOpts cOpts

concomitancePlay :: Opts -> Command -> IO ()
concomitancePlay gOpts cOpts = do
  ply' <- getPlay (fetch gOpts) (corpus gOpts) (play gOpts)
  case ply' of
    Nothing -> do
      hPutStrLn stderr "Could find play"
    Just ply -> do
      let scenes = map scnSpeakers $ plySegments ply
          characters = nub $ concat scenes
          characterSets = filter ((\l -> l>=2 && l<=(concMaxCardinalNum cOpts)) . length) $
            subsequences characters
          concomitanceValues = foldPlayWith concomitanceP characterSets scenes
          out = sortBy (concSortedBy cOpts) $ -- sortOn (sortOrder $ concSortedBy cOpts) $
            filter ((\v -> v >= (concLowerBoundConc cOpts) &&
                           v <= (concUpperBoundConc cOpts)) . snd) concomitanceValues
      hPutStrLn stderr $ "Count of scenes: " ++ (show $ length scenes)
      output gOpts out
  where
    sortBy MetricOrder = sortOn snd
    sortBy CardinalityOrder = sortOn (length . fst)
    sortBy _ = id

-- | Format the output
output :: (ToJSON a, Show a) => Opts -> [a] -> IO ()
output (Opts _ _ _ Raw _) vals = do
  mapM print vals
  return ()
output (Opts _ _ _ JSON _) vals = do
  print (toJSON vals)
-- output (Opts _ _ _ CSV _) vals = do
--   print vals
