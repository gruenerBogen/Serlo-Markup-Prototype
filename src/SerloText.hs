{-# LANGUAGE OverloadedStrings #-}
module SerloText
  (
    SerloText
  ) where

import Control.Monad (join)

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.String (fromString)
import Data.Text (unpack)

import qualified Data.HashMap.Strict as H

data SerloText = Paragraph [TextChild]
               | Heading Level [TextChild]
               | UnorderedList [ListItem]
               deriving (Eq, Show)

type Level = Int

type Inline = Bool
type Href = String

type ListItem = [TextChild]

data TextChild = Text String [TextProp]
               | Math String Inline [TextChild]
               | Link Href [TextChild]
               deriving (Eq, Show)

data TextProp = Strong
              | Emphasis
              deriving (Eq)

allTextProps :: [TextProp]
allTextProps = [Strong, Emphasis]

instance Show TextProp where
  show Strong = "strong"
  show Emphasis = "em"

instance ToJSON SerloText where
  toJSON = textToJSON

instance ToJSON TextChild where
  toJSON = childToJSON

textToJSON :: SerloText -> Value
textToJSON (Paragraph cs) = object [
                                     "type" .= String "p"
                                   , "children" .= toJSONList cs
                                   ]
textToJSON (Heading l cs) = object [
                                     "type" .= String "h"
                                   , "level" .= toJSON l
                                   , "children" .= toJSONList cs
                                   ]
textToJSON (UnorderedList cs) = object [
                                         "type" .= String "unordered-list"
                                       , "children" .= listItemsToJSON cs
                                       ]

listItemsToJSON :: [ListItem] -> Value
listItemsToJSON = toJSONList . map listItemToJSON

listItemToJSON :: ListItem -> Value
listItemToJSON i = object
  [
    "type" .= String "list-item"
  , "children" .= toJSONList
    [
      object [
               "type" .= String "list-item-child"
             , "children" .= map toJSON i
             ]
    ]
  ]

childToJSON :: TextChild -> Value
childToJSON (Text s ps) = object $ ["text" .= s] ++ map propToPair ps
  where propToPair :: TextProp -> Pair
        propToPair p = fromString (show p) .= Bool True
childToJSON (Math src inline text) = object
  [
    "type" .= String "math"
  , "src" .= src
  , "inline" .= inline
  , "children" .= toJSONList text
  ]
childToJSON (Link href text) = object
  [
    "type" .= String "a"
  , "href" .= href
  , "children" .= toJSONList text
  ]

instance FromJSON SerloText where
  parseJSON = parseSerloText

parseSerloText :: Value -> Parser SerloText
parseSerloText = withObject "SerloText" $ \v -> do
  t <- v .: "type"
  case unpack t of
    "p" -> Paragraph <$> parseChildren v
    "h" -> Heading <$> v .: "level" <*> parseChildren v
    "unordered-list" -> UnorderedList <$> (v .: "children" >>= parseListItems)
    _ -> fail "Text type not implemented."

parseListItems :: Value -> Parser [ListItem]
parseListItems v = parseJSONList v >>= mapM parseListItem

parseListItem :: Value -> Parser ListItem
parseListItem = withObject "ListItem" $ \v -> do
  c <- unpack <$> v .: "type"
  if c == "list-item-child"
    then parseChildren v
    else fail "Wrong item type."

instance FromJSON TextChild where
  parseJSON = parseTextChild

parseTextChild :: Value -> Parser TextChild
parseTextChild = withObject "TextChild" $ \v -> do
  text <- v .:? "text"
  case unpack <$> text of
    Just s ->  return $ Text s (props v)
    Nothing -> parseNonTextChild v

props :: Object -> [TextProp]
props v = [x | x <- allTextProps, ((fromString $ show x) `H.lookup` v) == Just (Bool True)]

parseNonTextChild :: Object -> Parser TextChild
parseNonTextChild v = do
  t <- unpack <$> v .: "type"
  case t of
    "math" -> Math <$> v .: "src" <*> v .: "inline" <*> parseChildren v
    "a" -> Link <$> v .: "href" <*> parseChildren v
    _ -> fail "Text shape/representation not implemented"

parseChildren :: Object -> Parser [TextChild]
parseChildren v = v .: "children" >>= parseJSONList
