module Text.AsciiDoc.Encode.Text where

import Text.PrettyPrint.HughesPJ hiding ((<>))

import Text.AsciiDoc.Types.Text

encodeFormattedText :: FormattedText -> Doc
encodeFormattedText = hcat . map encodeFormattedSegment

encodeFormattedSegment :: FormattedSegment -> Doc
encodeFormattedSegment (FormattedSegment s fs) = foldr applyFormatting (encodeContent s) fs

encodeContent :: Content -> Doc
encodeContent (Text s) = text $ escapeText s
encodeContent (Link t n) = text "link:" <> text t <> char '[' <> encodeFormattedText n <> char ']'
-- It is currently unclear, which notation is the best way to output LaTeX code.
encodeContent (Math _) = undefined

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
