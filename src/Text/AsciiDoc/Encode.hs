module Text.AsciiDoc.Encode
  (
    encodeAsciiDoc
  ) where

import Text.PrettyPrint.HughesPJ hiding ((<>))

import Text.AsciiDoc.Types (AsciiDoc(..))
import Text.AsciiDoc.Encode.Header (encodeHeader)
import Text.AsciiDoc.Encode.Block (encodeBlocks)

encodeAsciiDoc :: AsciiDoc -> Doc
encodeAsciiDoc (AsciiDoc header content) = maybe empty (encodeHeader) header
                                        <> encodeBlocks content
