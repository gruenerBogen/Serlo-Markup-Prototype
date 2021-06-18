module Text.AsciiDoc.Encode.Block
  (
    encodeBlock
  , encodeBlocks
  ) where

import Text.PrettyPrint.HughesPJ hiding ((<>))

import Text.AsciiDoc.Types.Block
import Text.AsciiDoc.Encode.Text (encodeFormattedText)

encodeBlock :: Block -> Doc
-- TODO: Also encode alignment
encodeBlock (Paragraph alignment content) = encodeFormattedText content
encodeBlock (Section level title content) = text (take (level + 1) $ repeat '=') <> char ' ' <> text title
                                         <> encodeBlocks content

encodeBlocks :: [Block] -> Doc
encodeBlocks = hcat . punctuate (text "\n\n") . map encodeBlock
