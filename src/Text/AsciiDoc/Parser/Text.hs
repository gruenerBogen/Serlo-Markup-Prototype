module Text.AsciiDoc.Parser.Text () where

import Control.Monad (liftM2)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)

import Text.AsciiDoc.Types.Text
import Text.AsciiDoc.Parser.Generic (skipWhitespace, p_attributeName)

delimiter = "*_\\#~^"

constrainedDelimiters = [ (Bold, "*")
                        , (Italic, "_")
                        ]

unconstrainedDelimiters = [ (Bold, "**")
                          , (Italic, "__")
                          , (Monospace, "``")
                          , (Highlight, "##")
                          , (Subscript, "~")
                          , (Superscript, "^")
                          ]

p_text :: Parser FormattedText
p_text = skipWhitespace *> p_formatted [] (fail "") p_endOfBlock

p_formatted :: [Formatting] -> Parser String -> Parser String -> Parser FormattedText
p_formatted fs d d' = p_formattedContent fs (d <|> d') <* d'

-- The parser-argument parses the end-delimiter
p_formattedContent :: [Formatting] -> Parser String -> Parser FormattedText
p_formattedContent fs d = (FormattedSegment fs <$> try p_macroCall) <:> p_formattedContent fs d
                      <|> try (lookAhead $ d) *> return [] -- End of Segment
                      <|> choice (p_unconstraineds fs d) <++> p_formattedContent fs d
                      <|> try (FormattedSegment fs <$> Text <$> choice
                               [ many1 p_unspecialChar
                               , try p_hardLineBreak
                               , p_whitespace
                               , count 1 anyChar
                               ]) <:> p_formattedContent fs d

p_unconstrainedDelim :: String -> Parser String
p_unconstrainedDelim s = string s *> return ""

p_unconstrained :: [Formatting] -> Parser String -> (Formatting, String) -> Parser FormattedText
p_unconstrained fs d (f,s) = let d' = p_unconstrainedDelim s
                             in try (d' *> p_formatted (f:fs) d d')

p_unconstraineds :: [Formatting] -> Parser String -> [Parser FormattedText]
p_unconstraineds fs d = map (p_unconstrained fs d) $ filter (not . (`elem` fs) . fst) unconstrainedDelimiters

--p_constrainedOpening :: String -> Parser String
--p_constrainedOpening s = count 1 (satisfy (not . isAlphaNum)) <* string s <* lookAhead alphaNum

p_unspecialChar = noneOf $ delimiter ++ "\n\r \t"
p_whitespace = many1 space *> return " "

p_hardLineBreak = many1 (oneOf " \t")
               *> char '+'
               *> endOfLine
               *> many (oneOf " \t")
               *> return "\n"

p_endOfBlock :: Parser String
p_endOfBlock = skipWhitespace *> eof *> return "\n"
           <|> skipWhitespace *> endOfLine *> (eof <|> (skipWhitespace <* endOfLine)) *> return "\n"

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
(<++>) = liftM2 (++)
