{-# LANGUAGE OverloadedStrings #-}
module Serlo.Text.ToJSON () where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.String (fromString)

import Serlo.Internal.Text ( SerloText(..)
                           , TextChild(..)
                           , ListItem
                           , TextProp
                           )

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
