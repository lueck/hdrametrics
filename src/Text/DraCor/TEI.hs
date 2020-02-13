{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Text.DraCor.TEI
  ( parseTEI
  , Speach (..)
  , TextState (..)
  , getTextStateText
  , isSpeaker
  , isSpeachOrStage
  ) where

-- | This module provides a SAX parser for TEI encoded theater plays.

import Xeno.SAX
import Xeno.Types
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Lens
import Text.Regex.TDFA
import Data.Char

-- * Types

-- | Wrappers for the types of text in a play.
data TextState = Mute T.Text | Aloud T.Text | Stage T.Text | Speaker T.Text
  deriving (Show, Eq)

-- | Unpack the 'Text' from a stateful 'TextState' text.
getTextStateText :: TextState -> T.Text
getTextStateText (Mute txt) = txt
getTextStateText (Aloud txt) = txt
getTextStateText (Stage txt) = txt
getTextStateText (Speaker txt) = txt

-- | A predicate that is 'True', if the 'TextState' of given text is
-- 'Speaker'. Useful for filtering.
isSpeaker :: TextState -> Bool
isSpeaker (Speaker _) = True
isSpeaker _ = False

-- | A predicate that is 'True', if the 'TextState' of given text is
-- 'Speach' of 'Stage'.
isSpeachOrStage :: TextState -> Bool
isSpeachOrStage (Aloud _) = True
isSpeachOrStage (Stage _) = True
isSpeachOrStage _ = False

-- | A speach (turn taking) of a theater play, with a (one or many)
-- speaker and a list of stateful speach texts.
data Speach = Speach
  { speachWho :: [T.Text]
  , speachSpeaker :: [T.Text]
  , speachSpeach :: [TextState]
  } deriving (Show, Eq)


-- * Internals of the SAX parser

data SaxState = SaxState
  { _tagStack :: [B.ByteString]
  , _inSp :: Bool
  , _textState :: [(T.Text -> TextState)]
  , _scenes :: [[Speach]]
  , _who :: T.Text
  , _speaker :: [T.Text]
  , _speach :: [TextState]
  }

makeLenses ''SaxState

initialSaxState :: Either String SaxState
initialSaxState = Right $ SaxState
  { _tagStack = []
  , _inSp = False
  , _textState = [Mute]
  , _scenes = [[]]
  , _who = ""
  , _speaker = []
  , _speach = []
  }

prefix :: String
prefix = "([a-zA-Z][[:alnum:]]?:)?"

-- FIXME: Add real handling of namespaces!
teiTag :: SaxState -> String -> String
teiTag s localName = prefix ++ localName ++ "$"

mkSpeach :: SaxState -> Speach
mkSpeach s = Speach
  { speachWho = T.words $ _who s
  , speachSpeaker = filter ((>0) . T.length) $
                    map (T.strip . (T.dropAround isPunctuation) . T.strip . getTextStateText) $
                    filter isSpeaker $ _speach s
  , speachSpeach = filter isSpeachOrStage $ _speach s
  }

toText :: B.ByteString -> T.Text
toText = T.decodeUtf8

openTag :: Either String SaxState -> B.ByteString -> Either String SaxState
openTag (Left s) tag = Left s
openTag (Right s) tag
  -- opening sp
  | tag =~ (teiTag s "sp") :: Bool
  = Right $ s & tagStack %~ (tag:)
    & inSp .~ True
    & who .~ ""
    & speaker .~ []
    & speach .~ []
  -- opening speaker in sp
  | (tag =~ (teiTag s "speaker") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ (tag:)
    & textState %~ (Speaker:)
  -- opening stage in sp
  | (tag =~ (teiTag s "stage") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ (tag:)
    & textState %~ (Stage:)
  -- opening p in sp
  | (tag =~ (teiTag s "p") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ (tag:)
    & textState %~ (Aloud:)
  -- opening p in lg
  | (tag =~ (teiTag s "lg") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ (tag:)
    & textState %~ (Aloud:)
  --
  | otherwise
  = Right $ s & tagStack %~ (tag:)


attrNode :: Either String SaxState -> B.ByteString -> B.ByteString -> Either String SaxState
attrNote (Left err) _ _ = Left err
attrNode (Right s) k v
  | _inSp s
    && k == "who"
  = Right $ s & who .~ (toText v)
  | otherwise = Right s


closeTag :: Either String SaxState -> B.ByteString -> Either String SaxState
closeTag (Left err) _ = Left err
closeTag (Right s) tag
  | tag /= head (_tagStack s)
  = Left "Fatal XML error: unexpected closing tag"
  | _tagStack s == []
  = Left "Fatal XML error: closing tag while no open tags left"
  -- closing sp
  | (tag =~ (teiTag s "sp") :: Bool)
  = Right $ s & tagStack %~ tail
    & scenes %~ (\xs -> init xs ++ [(last xs) ++ [mkSpeach s]])
    & who .~ ""
    & speaker .~ []
    & speach .~ []
    & inSp .~ False
  -- closing stage in sp
  | (tag =~ (teiTag s "stage") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ tail
    & textState %~ tail
  -- closing p in sp
  | (tag =~ (teiTag s "p") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ tail
    & textState %~ tail
  -- closing p in lg
  | (tag =~ (teiTag s "lg") :: Bool)
    && (_inSp s)
  = Right $ s & tagStack %~ tail
    & textState %~ tail
  --
  | otherwise
  = Right $ s & tagStack %~ tail

textNode :: Either String SaxState -> B.ByteString -> Either String SaxState
textNode (Left err) _ = Left err
textNode (Right s) str
  | _inSp s
  = Right $ s & speach %~ ((constructTxt s $ toText str):)
  | otherwise = Right s
  where
    constructTxt :: SaxState -> (T.Text -> TextState)
    constructTxt s = head $ _textState s

endOpenTag :: Either String SaxState -> B.ByteString -> Either String SaxState
endOpenTag s _ = s

cdata :: Either String SaxState -> B.ByteString -> Either String SaxState
cdata s _ = s


-- * The parser

-- | A parser for TEI encoded theater plays. A 'Left' value is
-- returned when a parser error has occurred. The speaches of the play
-- are wrapped into a 'Right' value.
parseTEI :: B.ByteString -> Either String [[Speach]]
parseTEI =
  fmap _scenes .
  joinEithers .
  fold openTag attrNode endOpenTag textNode closeTag cdata initialSaxState
  where
    joinEithers :: Either XenoException (Either String SaxState) -> Either String SaxState
    joinEithers (Left xerr) = Left $ show xerr
    joinEithers (Right result) = result
                

-- * Playing around

-- | Parse an example play with @stack runghc TEI.hs@
main :: IO ()
main = do
  let exampleDir = "../../../test/examples/"
      suff = "alberti-im-suff.xml"
  xml <- B.readFile $ exampleDir ++ suff
  let result = parseTEI xml
  case result of
    Left err -> do
      print err
    Right s -> do
      print s
      -- print $ map (map speachWho) s
      print $ length $ s --_scenes s
      print $ length $ head $ s --_scenes s
