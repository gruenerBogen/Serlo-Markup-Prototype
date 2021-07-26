module Text.AsciiDoc.Types.Text where

import Data.String (IsString(..))

import Text.AsciiDoc.Types.Generic (Attributes(..))

type FormattedText = [FormattedSegment]

data FormattedSegment = FormattedSegment [Formatting] Content
                      deriving (Eq, Show)

data Content = Text String
             | InlineMacroCall Name Target Attributes
             deriving (Eq, Show)

type Name = String
type Target = String

data Formatting = Bold
                | Italic
                | Monospace
                | Highlight
                | Subscript
                | Superscript
                deriving (Eq, Show)

-- Some default inline macros
latexMath :: String -> Content
latexMath src = InlineMacroCall "latexmath" "" $ Attributes [] [("src", src)]

asciiMath :: String -> Content
asciiMath src = InlineMacroCall "asciimath" "" $ Attributes [] [("src", src)]

instance IsString FormattedSegment where
  fromString = FormattedSegment [] . Text
