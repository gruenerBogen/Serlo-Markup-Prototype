module Text.AsciiDoc.Types.Block where

import Text.AsciiDoc.Types.Text (FormattedText)

data Block = Paragraph Alignment FormattedText
           | Section Level SectionTitle [Block]
           deriving (Eq, Show)

type Level = Int
type SectionTitle = String

data Alignment = AlignLeft
               | AlignCenter
               | AlignRight
               | AlignJustify
               deriving (Eq, Show)
