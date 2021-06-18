module Text.AsciiDoc.ToAsciiDoc
  (
    ToAsciiDoc(..)
  , ToBlock(..)
  , ToBlocks(..)
  ) where

import Text.PrettyPrint.HughesPJ (Doc)

import Text.AsciiDoc.Types (AsciiDoc, Block)
import Text.AsciiDoc.Encode (encodeAsciiDoc)

class ToAsciiDoc a where
  toAsciiDoc :: a -> AsciiDoc
  encodeAsAsciiDoc :: a -> Doc
  encodeAsAsciiDoc = encodeAsciiDoc . toAsciiDoc

instance ToAsciiDoc AsciiDoc where
  toAsciiDoc = id

class ToBlock a where
  toBlock :: a -> Block

class ToBlocks a where
  toBlocks :: a -> [Block]

instance ToBlock Block where
  toBlock = id

instance ToBlocks Block where
  toBlocks = (:[])
