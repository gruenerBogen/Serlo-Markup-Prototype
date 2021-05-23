module Serlo.Internal.Text
  (
    SerloText(..)
  , Level
  , Inline
  , Href
  , ListItem
  , TextChild(..)
  , TextProp(..)
  , allTextProps
  ) where

data SerloText = Paragraph [TextChild]
               | Heading Level [TextChild]
               | UnorderedList [ListItem]
               deriving (Eq, Show)

type Level = Int

type Inline = Bool
type Href = String

type ListItem = [TextChild]

data TextChild = Text String [TextProp]
               | Math String Inline [TextChild]
               | Link Href [TextChild]
               deriving (Eq, Show)

data TextProp = Strong
              | Emphasis
              deriving (Eq)

allTextProps :: [TextProp]
allTextProps = [Strong, Emphasis]

instance Show TextProp where
  show Strong = "strong"
  show Emphasis = "em"
