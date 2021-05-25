module Text.AsciiDoc.Types.Text where

import Text.AsciiDoc.Types.Generic (Attributes)

type FormattedText = [FormattedSegment]

data FormattedSegment = FormattedSegment [Formatting] Content
                      deriving (Eq, Show)

data Content = Text String
             | InlineMacroCall Name Target PositionalAttributes NamedAttributes
             deriving (Eq, Show)

type Name = String
type Target = String

type PositionalAttributes = [String]
type NamedAttributes = [(String, String)]

data Formatting = Bold
                | Italic
                | Monospace
                | Highlight
                | Subscript
                | Superscript
                deriving (Eq, Show)
