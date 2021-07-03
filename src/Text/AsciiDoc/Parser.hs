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

-- Das sind ab hier nur einige Notes:

-- Das müsste noch ein vernünftiger Parser-Typ werden
--data AsciiDocParser a = AsciiDocParser a

--parseAsciiDoc :: AsciiDocParser a -> String -> Either ?? a
--parseAsciiDoc = undefined

--class FromAsciiDoc a where
--  asciiDocParser :: AsciiDocParser a
--  decodeAsciiDoc :: String -> Either ?? a
--  decodeAsciiDoc = parseAsciiDoc asciiDocParser
