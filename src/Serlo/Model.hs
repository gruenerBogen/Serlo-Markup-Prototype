module Serlo.Model
  (
    SerloPlugin
  , SerloContent
  , name
  ) where

import Serlo.Text
import Serlo.Internal.Model (SerloPlugin(..), SerloContent)

name :: SerloPlugin -> String
name (RichText _) = "text"
name (Rows _) = "rows"
name (Spoiler _ _) = "spoiler"
name (Injection _) = "injection"
name (Image _ _) = "image"
