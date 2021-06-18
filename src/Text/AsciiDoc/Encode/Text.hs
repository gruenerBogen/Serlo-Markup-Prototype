module Text.AsciiDoc.Encode.Text
  (
    encodeFormattedText
  ) where

import Text.PrettyPrint.HughesPJ hiding ((<>))

import Text.AsciiDoc.Types.Text

encodeFormattedText :: FormattedText -> Doc
encodeFormattedText = hcat . map encodeFormattedSegment

encodeFormattedSegment :: FormattedSegment -> Doc
encodeFormattedSegment (FormattedSegment fs s) = foldr applyFormatting (encodeContent s) fs

encodeContent :: Content -> Doc
encodeContent (Text s) = text $ escapeText s
encodeContent (InlineMacroCall n t pattrs nattrs) =
  case n of
    "latexmath" -> text "\\(" <> text (maybe "" id $ lookup "src" nattrs) <> text "\\)"
    "asciimath" -> enclose (text "\\$") $ text $ maybe "" id $ lookup "src" nattrs
    _           -> text n <> char ':' <> text t <> char '[' <> encodeAttrs (pattrs ++ map encodeNamedAttr nattrs) <> char ']'

encodeAttrs :: [String] -> Doc
encodeAttrs = hsep . punctuate (char ',') . map (text . escapeAttr)

encodeNamedAttr :: (String, String) -> String
encodeNamedAttr (k,v) =  k ++ "=" ++ escapeAttr v

-- TODO: Handle case when quotes are needed
escapeAttr :: String -> String
escapeAttr = id

escapeText :: String -> String
--escapeText = replace "\n" " +\n"
escapeText = id

enclose :: Doc -> Doc -> Doc
enclose enc mid = enc <> mid <> enc

applyFormatting :: Formatting -> Doc -> Doc
applyFormatting Bold = enclose $ text "**"
applyFormatting Italic = enclose $ text "__"
applyFormatting Monospace = enclose $ text "``"
applyFormatting Highlight = enclose $ text "##"
applyFormatting Subscript = enclose $ char '~'
applyFormatting Superscript = enclose $ char '^'
