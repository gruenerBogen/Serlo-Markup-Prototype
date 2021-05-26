module Text.AsciiDoc.Types
  (
    AsciiDoc(..)
  ) where

import Text.AsciiDoc.Types.Header
import Text.AsciiDoc.Types.Block

data AsciiDoc = AsciiDoc (Maybe Header) [Block]
              deriving (Eq, Show)
