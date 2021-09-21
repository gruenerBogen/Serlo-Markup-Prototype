module Serlo.Internal.Model
  (
    SerloPlugin (..)
  , SerloContent
  , name
  ) where

import Serlo.Text (SerloText)

data SerloPlugin = RichText [SerloText] -- contnet
                 | Image String String -- src alt
                 | Spoiler String SerloPlugin -- title content
                 | Rows [SerloPlugin]
                 | Injection Int -- id
                 | Important SerloPlugin
--                 | Article SerloPlugin SerloPlugin [Int] ExerciseFolder RelatedContent [String]
--                 -- introduction content exercises exerciseFolder relatedContent sources
--                 | ArticleIntroduction SerloPlugin SerloPlugin Bool Int -- explanation multimedia illustrating width
                 deriving (Eq, Show)

type SerloContent = SerloPlugin

--data ExerciseFolder = ExerciseFolder String String -- id title
--data RelatedContent = RelatedContent [String] [String] [String] -- articles courses videos

name :: SerloPlugin -> String
name (RichText _) = "text"
name (Rows _) = "rows"
name (Spoiler _ _) = "spoiler"
name (Injection _) = "injection"
name (Image _ _) = "image"
name (Important _) = "important"
--name (Article _ _ _ _ _ _) = "article")
--name (ArticleIntroduction _ _ _ _) = "articleIntroduction"
