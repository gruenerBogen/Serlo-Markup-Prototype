module Text.AsciiDoc.Parser.Block
  (
    p_block
  ) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)

import Text.AsciiDoc.Types.Block
import Text.AsciiDoc.Parser.Text (p_text)

-- These imports are temporary until proper parser are written
import Text.AsciiDoc.Types.Generic (Attributes(..))
import Text.AsciiDoc.Types.Text (FormattedSegment(..), Content(..))

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
  return $ Block { context = "section"
                 , title = Just [FormattedSegment [] (Text t)]
                 , attributes = Attributes [] [("level", show l)]
                 , content = Compount c
                 }

-- TODO: Should fail when p_text is empty.
p_paragraph :: Parser Block
p_paragraph = do
  c <- p_text
  if c == []
    then fail "Empty paragraph."
    else return $ Block { context = "paragraph"
                        , title = Nothing
                        , attributes = Attributes [] []
                        , content = Simple c
                        }

skipToStart :: Parser ()
skipToStart = spaces
