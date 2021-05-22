{-# LANGUAGE OverloadedStrings #-}
module SerloJSON where

import Data.Aeson
import Data.Aeson.Types (Parser)

import Data.Text (Text, unpack)
import Text.Read (readMaybe)

import qualified Data.String as S
import qualified Data.Vector as V

import Serlo.Model

instance ToJSON SerloPlugin where
  toJSON = pluginToJSON

instance FromJSON SerloPlugin where
  parseJSON = pluginFromJSON

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


pluginFromJSON :: Value -> Parser SerloPlugin
pluginFromJSON = withObject "SerloPlugin" $ \v -> do
  n <- v .: "plugin" :: Parser Text
  s <- v .: "state" :: Parser Value
  pluginFromJSONConstructor n s

pluginFromJSONConstructor :: Text -> Value -> Parser SerloPlugin
pluginFromJSONConstructor name =
  case unpack name of
    "text" -> fmap RichText . parseJSONList
    "image" -> parseImage
    "spoiler" -> parseSpoiler
    "rows" -> parseRows
    "injection" -> parseInjection
    _ -> \_ -> fail $ "Plguin '" ++ (unpack name) ++ "' not implemented."

parseImage :: Value -> Parser SerloPlugin
parseImage = undefined

parseSpoiler :: Value -> Parser SerloPlugin
parseSpoiler = withObject "Spoiler" $ \v ->
  Spoiler <$> unpack `fmap` (v .: "title") <*> (parseJSON =<< (v .: "content"))

parseRows :: Value -> Parser SerloPlugin
parseRows = fmap Rows . parseJSONList

parseInjection :: Value -> Parser SerloPlugin
parseInjection = withText "Injection" $ \t ->
  case (readMaybe . tail . unpack) t of
    Just ref -> return $ Injection ref
    Nothing -> fail "Invalid content of injection."
