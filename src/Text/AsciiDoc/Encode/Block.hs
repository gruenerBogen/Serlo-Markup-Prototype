module Text.AsciiDoc.Encode.Block
  (
    encodeBlock
  , encodeBlocks
  ) where

import Text.Read (readMaybe)
import Text.PrettyPrint.HughesPJ hiding ((<>))

import Text.AsciiDoc.Types.Generic ( Attributes(..)
                                   , attributesEmpty
                                   , lookupNamed
                                   )
import Text.AsciiDoc.Types.Block
import Text.AsciiDoc.Types.Text (FormattedText)
import Text.AsciiDoc.Encode.Text (encodeFormattedText)

encodeBlock :: Block -> Doc
encodeBlock b = maybeEncodeAttributes (attributes b)
             <> case context b of
                 "section" -> encodeAsSection b
                 _ -> encodeGenericBlock b

encodeAsSection :: Block -> Doc
encodeAsSection b = let level = (maybe 0 id $ readMaybe =<< (lookupNamed "level" $ attributes b))
                        t = maybe [] id (title b)
                    in text (take (level + 1) $ repeat '=')
                    <> char ' '
                    <> encodeFormattedText t
                    -- This \n should not be part of the title, but currently
                    -- it is. Hence it is currently commented out.
                    -- <> char '\n'
                    <> encodeContent (content b)

-- TODO: Do something with the context
encodeGenericBlock :: Block -> Doc
encodeGenericBlock b = encodeBlockTitle (title b)
                    <> encodeContent (content b)

encodeContent :: BlockContent -> Doc
encodeContent (Compount bs) = encodeBlocks bs
encodeContent (Simple ft) = encodeFormattedText ft
encodeContent (Raw s) = text s
encodeContent Empty = empty

maybeEncodeAttributes :: Attributes -> Doc
maybeEncodeAttributes a = if (attributesEmpty $ a)
                          then empty
                          else encodeAttributes a <> char '\n'

encodeAttributes :: Attributes -> Doc
encodeAttributes (Attributes pos named) = brackets $ hcat $ punctuate (char ',') $ map text pos <> map encNamed named
  where encNamed (k, v) = text k <> char '=' <> text v

encodeBlockTitle :: Maybe FormattedText -> Doc
encodeBlockTitle = maybe empty (sourround (char '.') (char '\n') . encodeFormattedText)

encodeBlocks :: [Block] -> Doc
encodeBlocks = hcat . punctuate (text "\n\n") . map encodeBlock

sourround :: Doc -> Doc -> Doc -> Doc
sourround start end mid = start <> mid <> end
