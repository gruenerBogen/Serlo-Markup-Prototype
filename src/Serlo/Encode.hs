module Serlo.Encode () where

import Serlo.Internal.Model (SerloPlugin(..), SerloContent)
import Serlo.Internal.Text

import Text.AsciiDoc.Types (AsciiDoc(..), Block, FormattedSegment(..))
import Text.AsciiDoc.ToAsciiDoc (ToAsciiDoc(..), ToBlocks(..))
import qualified Text.AsciiDoc.Types as A

-- This should late be SerloContent (when it is its own data type and not
-- just a type synonym).
instance ToAsciiDoc SerloPlugin where
  toAsciiDoc = (AsciiDoc Nothing) . toBlocks

instance ToBlocks SerloPlugin where
  toBlocks = pluginToBlocks

pluginToBlocks :: SerloPlugin -> [Block]
pluginToBlocks (RichText ts) = concat $ map toBlocks ts
pluginToBlocks (Rows ps) = concat $ map toBlocks ps

instance ToBlocks SerloText where
  toBlocks = textToBlocks

textToBlocks :: SerloText -> [Block]
textToBlocks (Paragraph cs) = (:[]) $ A.paragraph (map toFormattedSegment cs)
-- Serlo and AsciiDoc count heading levels differently Serlo denotes the title
-- level with 1 and AsciiDoc with 0.
textToBlocks (Heading l cs) = (:[]) $ A.section (l-1) (map toFormattedSegment cs) []

toFormattedSegment :: TextChild -> FormattedSegment
toFormattedSegment (Text c ps) = FormattedSegment (map p2p ps) (A.Text c)
toFormattedSegment (Math src _ _) = FormattedSegment [] (A.latexMath src)

p2p :: TextProp -> A.Formatting
p2p Strong = A.Bold
p2p Emphasis = A.Italic