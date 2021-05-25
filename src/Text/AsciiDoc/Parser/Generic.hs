module Text.AsciiDoc.Parser.Generic
  (
    skipWhitespace
  ) where

import Text.Parsec (skipMany)
import Text.Parsec.Char (oneOf)
import Text.Parsec.String (Parser)

skipWhitespace :: Parser ()
skipWhitespace = skipMany $ oneOf " \t"
