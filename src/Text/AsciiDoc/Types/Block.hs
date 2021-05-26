module Text.AsciiDoc.Types.Block where

import Text.AsciiDoc.Types.Text (FormattedText)

data Block = Paragraph Alignment FormattedText
           | Section Level Title [Block]
           deriving (Eq, Show)

type Level = Int
type Title = String

data Alignment = AlignLeft
               | AlignCenter
               | AlignRight
               | AlignJustify
               deriving (Eq, Show)
