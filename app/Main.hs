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
-- import qualified Data.Csv as Csv

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


fetch' :: String -> String -> IO B.ByteString
-- fetch url path = do
--   request <- parseRequest $ url ++ path
--   manager <- newManager tlsManagerSettings
--   return $ responseBody $ httpLbs request manager
fetch' url path = simpleHttp $ url ++ path

fetch :: Opts -> (String -> IO B.ByteString)
fetch opts = fetch' $ url opts

run :: Opts -> IO ()
run gOpts@(Opts _ _ _ _ cOpts@(Concomitance _ _ _ _ _)) =
  concomitanceLikePlay concomitanceP gOpts cOpts
run gOpts@(Opts _ _ _ _ cOpts@(Dominance _ _ _ _ _)) =
  concomitanceLikePlay dominanceP gOpts cOpts
run gOpts@(Opts _ _ _ _ cOpts@(Cooccurrence _ _ _ _ _)) =
  concomitanceLikePlay cooccurrenceP gOpts cOpts

-- concomitanceLikePlay :: Eq a => ([a] -> [a] -> Bool) -> Opts -> Command -> IO ()
concomitanceLikePlay predicate gOpts cOpts = do
  ply' <- getPlay (fetch gOpts) (corpus gOpts) (play gOpts)
  case ply' of
    Nothing -> do
      hPutStrLn stderr "Could find play"
    Just ply -> do
      let scenes = map scnSpeakers $ plySegments ply
          characters = nub $ concat scenes
          -- characterSets = filter ((\l -> l>=2 && l<=(concMaxCardinalNum cOpts)) . length) $
          --   subsequences characters
          characterSets = filter longerThanOne $
            subsequencesOfSize (concMaxCardinalNum cOpts) characters
          concomitanceValues = foldPlayWith predicate characterSets scenes
          out = sortBy (concSortedBy cOpts) $ -- sortOn (sortOrder $ concSortedBy cOpts) $
            filter ((\v -> v >= (concLowerBoundConc cOpts) &&
                           v <= (concUpperBoundConc cOpts)) . snd) $
            map (mkAbsoluteValues (concAbsoluteValues cOpts) (fromIntegral $ length scenes)) -- mk absolute values
            concomitanceValues
      hPutStrLn stderr $ "Found scenes: " ++ (show $ length scenes)
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
output :: (ToJSON a, Show a) => Opts -> [a] -> IO ()
output (Opts _ _ _ Raw _) vals = do
  mapM print vals
  return ()
output (Opts _ _ _ JSON _) vals = do
  print (toJSON vals)
-- output (Opts _ _ _ CSV _) vals = do
--   print vals
