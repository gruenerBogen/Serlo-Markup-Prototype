module Text.AsciiDoc.Encode.Generic
  (
    encodeSectionHeading
  ) where

import Text.PrettyPrint.HughesPJ hiding ((<>))

encodeSectionHeading :: Int -> String -> Doc
encodeSectionHeading n title = text (take (n + 1) $ repeat '=') <> char ' ' <> encodeText title

encodeText :: String -> Doc
encodeText = text
