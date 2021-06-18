module Text.AsciiDoc.Types
  (
    AsciiDoc(..)
  , module Text.AsciiDoc.Types.Generic
  , module Text.AsciiDoc.Types.Header
  , module Text.AsciiDoc.Types.Block
  , module Text.AsciiDoc.Types.Text
  ) where

import Text.AsciiDoc.Types.Header
import Text.AsciiDoc.Types.Block
import Text.AsciiDoc.Types.Generic
import Text.AsciiDoc.Types.Text

data AsciiDoc = AsciiDoc (Maybe Header) [Block]
              deriving (Eq, Show)
