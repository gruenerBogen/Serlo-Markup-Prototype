module Serlo.Internal.Model
  (
    SerloPlugin (..)
  , SerloContent
  , name
  ) where

import Serlo.Text (SerloText)

data SerloPlugin = RichText [SerloText] -- contnet
                 | Image String Int -- src maxWitdh
                 | Spoiler String SerloPlugin -- title content
                 | Rows [SerloPlugin]
                 | Injection Int -- id
                 deriving (Eq, Show)

type SerloContent = SerloPlugin

name :: SerloPlugin -> String
name (RichText _) = "text"
name (Rows _) = "rows"
name (Spoiler _ _) = "spoiler"
name (Injection _) = "injection"
name (Image _ _) = "image"
