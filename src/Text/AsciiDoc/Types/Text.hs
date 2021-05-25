module Text.AsciiDoc.Types.Text where

import Text.AsciiDoc.Types.Generic (Attributes)

type FormattedText = [FormattedSegment]

data FormattedSegment = FormattedSegment [Formatting] Content
                      deriving (Eq, Show)

data Content = Text String
             | Link Target Name
             | Math String
             deriving (Eq, Show)

type Name = FormattedText
type Target = String

data Formatting = Bold
                | Italic
                | Monospace
                | Highlight
                | Subscript
                | Superscript
                deriving (Eq, Show)
