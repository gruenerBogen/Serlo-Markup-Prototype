module Text.AsciiDoc.Parser.Header
  (
    p_header
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char

import Text.AsciiDoc.Types.Header
import Text.AsciiDoc.Parser.Generic (skipWhitespace, p_attributeName)

p_header :: Parser Header
p_header = Header <$> optionMaybeTry p_title
                  <*> optionMaybeTry p_authorRev
                  <*> many p_attribute
                  <*  endOfLine

p_title :: Parser String
p_title = string "= " *> line

p_authorRev :: Parser AuthorRevision
p_authorRev = AuthorRevision <$> p_authors
                             <*> optionMaybeTry p_revision

p_authors = (p_author `sepBy1` (char ';' *> skipWhitespace)) <* endOfLine

p_author :: Parser Author
p_author = Author <$> name <*> mail
  where name = many (noneOf "\n\r<;")
        mail = optionMaybe $ try $ p_mail

p_revision :: Parser Revision
p_revision = Revision <$> (char 'v' *> p_revNr <* skipWhitespace <* endOfLine) <*> return Nothing <*> return Nothing
         <|> Revision <$> (p_revNr <* spaces) <*> p_revDate <*> p_revRemark

p_revNr :: Parser RevNr
p_revNr = many1 (noneOf "\n\r ,:")

p_revDate :: Parser (Maybe RevDate)
p_revDate = optionMaybe $ try $ string ", " *> many (noneOf "\n\r:")

p_revRemark :: Parser (Maybe RevRemark)
p_revRemark = Just <$> (string ": " *> skipWhitespace *> line)
          <|> endOfLine *> return Nothing

p_mail :: Parser String
p_mail = char '<' *> many (noneOf ">") <* char '>'

-- Multiline Attribute lists are still missing
p_attribute :: Parser (String, String)
p_attribute = ((,)) <$> (char ':' *> p_attributeName <* char ':' <* many1 space) <*> line

line = many (noneOf "\n\r") <* endOfLine

optionMaybeTry = optionMaybe . try
