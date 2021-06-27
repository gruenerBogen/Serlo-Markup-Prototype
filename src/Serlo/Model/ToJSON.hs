{-# LANGUAGE OverloadedStrings #-}
module Serlo.Model.ToJSON () where

import Data.Aeson
import Data.Aeson.Types (Parser)

import qualified Data.Vector as V

import Serlo.Internal.Model (SerloPlugin(..), name)

instance ToJSON SerloPlugin where
  toJSON = pluginToJSON

pluginToJSON :: SerloPlugin -> Value
pluginToJSON p = object ["plugin" .= (name p), "state" .= (state p)]

state :: SerloPlugin -> Value
state (RichText t) = toJSONList t
state (Rows ps) = rowsToJSON ps
state (Spoiler title content) = spoilerToJSON title content
state _ = String "test"

rowsToJSON :: [SerloPlugin] -> Value
rowsToJSON = Array . V.fromList . map pluginToJSON

spoilerToJSON :: String -> SerloPlugin -> Value
spoilerToJSON title content = object ["title" .= title, "content" .= pluginToJSON content]
