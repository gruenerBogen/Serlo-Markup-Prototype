module Serlo.Encode () where

import Data.String (fromString)

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
pluginToBlocks (Spoiler t c) = [A.Block { A.context = "spoiler"
                                        , A.attributes = A.emptyAttributes
                                        , A.title = Just [fromString t]
                                        , A.content = A.Compount (pluginToBlocks c)
                                        }]
pluginToBlocks (Injection i) = [A.blockMacroCall "injection" (Just $ show i) A.emptyAttributes]
pluginToBlocks (Image src alt) = [A.blockMacroCall "image" (Just src) (A.fromAList [("alt", alt)])]
pluginToBlocks (Important c) = [A.Block { A.context = "important"
                                        , A.attributes = A.emptyAttributes
                                        , A.title = Nothing
                                        , A.content = A.Compount (pluginToBlocks c)
                                        }]

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
