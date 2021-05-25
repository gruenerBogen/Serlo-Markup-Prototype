module Text.AsciiDoc.Parser.Text () where

import Control.Monad (liftM2)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)

import Text.AsciiDoc.Types.Text
import Text.AsciiDoc.Parser.Generic (skipWhitespace)

delimiter = "*_\\#~^"

p_text :: Parser FormattedText
p_text = skipWhitespace *> p_textMiddle

p_textMiddle :: Parser FormattedText
p_textMiddle = (FormattedSegment [] <$> Text <$> many1 p_unspecialChar) <:> p_textMiddle
           <|> try (p_endOfBlock *> return [])
           <|> try (FormattedSegment [] <$> Text <$> p_hardLineBreak) <:> p_textMiddle
           <|> (FormattedSegment [] <$> Text <$> p_whitespace) <:> p_textMiddle

p_unspecialChar = noneOf $ delimiter ++ "\n\r \t"
p_whitespace = many1 space *> return " "

p_hardLineBreak = many1 (oneOf " \t")
               *> char '+'
               *> endOfLine
               *> many (oneOf " \t")
               *> return "\n"

p_endOfBlock :: Parser Char
p_endOfBlock = eof *> return '\n'
           <|> skipWhitespace *> endOfLine *> (eof <|> (skipWhitespace <* endOfLine)) *> return '\n'

(<:>) = liftM2 (:)

p_boldUnconstrained :: Parser String
p_boldUnconstrained = between (string "**") (string "**") $ many p_boldContent

p_boldContent :: Parser Char
p_boldContent = noneOf "*\n\r"
            <|> try (char '*' <* lookAhead (noneOf "*"))
            <|> try (endOfLine <* lookAhead (noneOf "\r\n"))
