module Text.AsciiDoc.Parser
  (
    p_asciiDoc
  ) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)

import Text.AsciiDoc.Types (AsciiDoc(..))

import Text.AsciiDoc.Parser.Header (p_header)
import Text.AsciiDoc.Parser.Block (p_block)

p_asciiDoc :: Parser AsciiDoc
p_asciiDoc = AsciiDoc <$> (optionMaybe $ try $ p_header) <*> many p_block
