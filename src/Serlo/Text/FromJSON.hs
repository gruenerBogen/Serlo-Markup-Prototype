{-# LANGUAGE OverloadedStrings #-}
module Serlo.Text.FromJSON () where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.String (fromString)
import Data.Text (unpack)

import qualified Data.HashMap.Strict as H

import Serlo.Internal.Text ( SerloText(..)
                           , TextChild(..)
                           , ListItem
                           , TextProp
                           , allTextProps
                           )

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
