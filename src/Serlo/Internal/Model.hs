module Serlo.Internal.Model
  (
    SerloPlugin (..)
  , SerloContent
  ) where

import Serlo.Text (SerloText)

data SerloPlugin = RichText [SerloText] -- contnet
                 | Image String Int -- src maxWitdh
                 | Spoiler String SerloPlugin -- title content
                 | Rows [SerloPlugin]
                 | Injection Int -- id
                 deriving (Eq, Show)

type SerloContent = SerloPlugin
