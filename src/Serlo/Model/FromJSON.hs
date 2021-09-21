{-# LANGUAGE OverloadedStrings #-}
module Serlo.Model.FromJSON () where

import Data.Aeson
import Data.Aeson.Types (Parser)

import Data.Text (Text, unpack)
import Text.Read (readMaybe)

import Serlo.Internal.Model (SerloPlugin(..))

instance FromJSON SerloPlugin where
  parseJSON = pluginFromJSON

pluginFromJSON :: Value -> Parser SerloPlugin
pluginFromJSON = withObject "SerloPlugin" $ \v -> do
  n <- v .: "plugin" :: Parser Text
  s <- v .: "state" :: Parser Value
  case n of
    "article" -> parseArticleContent s >>= pluginFromJSON
    _         -> pluginFromJSONConstructor n s

pluginFromJSONConstructor :: Text -> Value -> Parser SerloPlugin
pluginFromJSONConstructor name =
  case unpack name of
    "text" -> fmap RichText . parseJSONList
    "image" -> parseImage
    "spoiler" -> parseSpoiler
    "rows" -> parseRows
    "injection" -> parseInjection
    "important" -> parseImportant
    n -> \_ -> fail $ "Plguin '" ++ n ++ "' not implemented."

parseImage :: Value -> Parser SerloPlugin
parseImage = withObject "Image" $ \v ->
  Image <$> unpack `fmap` (v .: "src") <*> unpack `fmap` (v .: "alt")

parseSpoiler :: Value -> Parser SerloPlugin
parseSpoiler = withObject "Spoiler" $ \v ->
  Spoiler <$> unpack `fmap` (v .: "title") <*> (parseJSON =<< (v .: "content"))

parseRows :: Value -> Parser SerloPlugin
parseRows = fmap Rows . parseJSONList

parseInjection :: Value -> Parser SerloPlugin
parseInjection = withText "Injection" $ \t ->
  case (readMaybe . unpack) t of
    Just ref -> return $ Injection ref
    Nothing -> fail "Invalid content of injection."

parseImportant :: Value -> Parser SerloPlugin
parseImportant v = Important <$> pluginFromJSON v

parseArticleContent :: Value -> Parser Value
parseArticleContent = withObject "Article" $ \v ->
  parseJSON =<< (v .: "content")
