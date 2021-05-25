module Text.AsciiDoc.Encode.Text where

import Text.PrettyPrint.HughesPJ hiding ((<>))

import Text.AsciiDoc.Types.Text

encodeFormattedText :: FormattedText -> Doc
encodeFormattedText = hcat . map encodeFormattedSegment

encodeFormattedSegment :: FormattedSegment -> Doc
encodeFormattedSegment (FormattedSegment fs s) = foldr applyFormatting (encodeContent s) fs

encodeContent :: Content -> Doc
encodeContent (Text s) = text $ escapeText s

escapeText :: String -> String
--escapeText = replace "\n" " +\n"
escapeText = id

enclose :: Doc -> Doc -> Doc
enclose enc mid = enc <> mid <> enc

applyFormatting :: Formatting -> Doc -> Doc
applyFormatting Bold = enclose $ text "**"
applyFormatting Italic = enclose $ text "__"
applyFormatting Monospace = enclose $ text "``"
applyFormatting Highlight = enclose $ char '#'
applyFormatting Subscript = enclose $ char '~'
applyFormatting Superscript = enclose $ char '^'
