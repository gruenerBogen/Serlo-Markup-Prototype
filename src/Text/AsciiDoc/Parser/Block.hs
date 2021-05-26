module Text.AsciiDoc.Parser.Block () where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)

import Text.AsciiDoc.Types.Block
import Text.AsciiDoc.Parser.Text (p_text)

p_block :: Parser Block
p_block = skipToStart *> choice
  [ try p_section
  , p_paragraph
  ]

p_section :: Parser Block
p_section = do
  char '='
  l <- length <$> many (char '=')
  char ' '
  t <- many (noneOf "\r\n")
  c <- many p_block
  return $ Section l t c

-- TODO: Should fail when p_text is empty.
p_paragraph :: Parser Block
p_paragraph = do
  c <- p_text
  if c == []
    then fail "Empty paragraph."
    else return $ Paragraph AlignLeft c

skipToStart :: Parser ()
skipToStart = spaces
