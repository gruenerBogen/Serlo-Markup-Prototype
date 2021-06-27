module Serlo.Model
  (
    SerloPlugin
  , SerloContent
  , name
  , module Serlo.Model.FromJSON
  , module Serlo.Model.ToJSON
  ) where

import Serlo.Text
import Serlo.Internal.Model (SerloPlugin(..), SerloContent, name)
import Serlo.Model.FromJSON
import Serlo.Model.ToJSON
