module Text.AsciiDoc.Parser.Text () where

import Control.Monad (liftM2)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)

import Text.AsciiDoc.Types.Text
import Text.AsciiDoc.Parser.Generic (skipWhitespace, p_attributeName)

delimiter = "*_\\#~^"

p_text :: Parser FormattedText
p_text = skipWhitespace *> p_textMiddle

p_textMiddle :: Parser FormattedText
p_textMiddle = (FormattedSegment [] <$> try p_macroCall) <:> p_textMiddle
           <|> (FormattedSegment [] <$> Text <$> many1 p_unspecialChar) <:> p_textMiddle
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
p_endOfBlock = skipWhitespace *> eof *> return '\n'
           <|> skipWhitespace *> endOfLine *> (eof <|> (skipWhitespace <* endOfLine)) *> return '\n'

p_macroCall :: Parser Content
p_macroCall = do
  n <- p_attributeName
  char ':'
  t <- many $ noneOf "[ \n\t\r"
  (pAttr, nAttr) <- between (char '[') (char ']') p_macroAttrs
  return $ InlineMacroCall n t pAttr nAttr

p_macroAttrs :: Parser (PositionalAttributes, NamedAttributes)
p_macroAttrs = ((,)) <$> p_posAttrs <*> p_namedAttrs

p_posAttrs :: Parser PositionalAttributes
p_posAttrs = try (lookAhead p_namedAttr) *> return []
         <|> (skipWhitespace *> p_attributeValue)  <:> (skipWhitespace *> (char ',' *> p_posAttrs) <|> return [])
         <|> return []

p_namedAttrs :: Parser NamedAttributes
p_namedAttrs = p_namedAttr `sepBy` (skipWhitespace *> char ',')

p_namedAttr :: Parser (String, String)
p_namedAttr = ((,)) <$> (skipWhitespace *> p_attributeName)
                   <*> (skipWhitespace *> char '=' *> skipWhitespace *> p_attributeValue)

p_attributeValue :: Parser String
p_attributeValue = between (char '"') (char '"') p_quotedAttributeValue
               <|> many1 (noneOf "]\n\t \r\",")

-- TODO: Check if the \\ behaves as in the spec
p_quotedAttributeValue = many1 $ noneOf "\"\\"
                             <|> string "\\\"" *> return '"'
                             <|> string "\\\\" *> return '\\'

(<:>) = liftM2 (:)

p_boldUnconstrained :: Parser String
p_boldUnconstrained = between (string "**") (string "**") $ many p_boldContent

p_boldContent :: Parser Char
p_boldContent = noneOf "*\n\r"
            <|> try (char '*' <* lookAhead (noneOf "*"))
            <|> try (endOfLine <* lookAhead (noneOf "\r\n"))
