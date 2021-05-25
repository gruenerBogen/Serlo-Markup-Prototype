module Text.AsciiDoc.Parser.Generic
  (
    skipWhitespace
  , p_attributeName
  ) where

import Data.Char (toLower)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)

skipWhitespace :: Parser ()
skipWhitespace = skipMany $ oneOf " \t"

p_attributeName :: Parser String
p_attributeName = do
    f <- oneOf wordChar
    r <- many (oneOf wordChar <|> char '-')
    return $ map toLower (f:r)
  where wordChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
