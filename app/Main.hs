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

import Text.DraCor.Types
import Text.DraCor.API

import Text.DraCor.Concomitance

data Opts = Opts
  { url :: String
  , corpus :: String
  , play :: String
  , comnd :: Command
  }

data Command = Concomitance
  { concMaxCardinalNum :: Int
  , concLowerBoundThreshold :: Float
  , concUpperBoundThreshold :: Float
  }
  

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
  <*> subparser
  (command "concomitance" (info (helper <*> concomitance_)
                           (progDesc "Calculate concomitance measures")))

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
                   <> metavar "LOWERBOUND"
                   <> value 0.0
                   <> showDefault)
  <*> option auto (long "upperbound"
                   <> short 'u'
                   <> help "Upper bound threshold for the output. Character sets with a concomitance bigger than this threshold will be removed from the output."
                   <> metavar "UPPERBOUND"
                   <> value 1.0
                   <> showDefault)

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
run gOpts@(Opts _ _ _ cOpts@(Concomitance _ _ _)) = concomitancePlay gOpts cOpts

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
          output = filter ((\v -> v >= (concLowerBoundThreshold cOpts) &&
                                  v <= (concUpperBoundThreshold cOpts)) . snd) concomitanceValues
      hPutStrLn stderr $ "Count of scenes: " ++ (show $ length scenes)
      mapM print output
      return ()
      
