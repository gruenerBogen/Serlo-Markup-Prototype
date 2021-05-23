module Serlo.Markup.TextToMarkup
  (
    pp_text
  ) where

import Text.PrettyPrint.HughesPJ hiding ((<>))

import Serlo.Internal.Text ( SerloText(..)
                           , TextChild(..)
                           , TextProp(..))
import Serlo.Markup.ToMarkup (pp_enclose)

pp_text :: SerloText -> Doc
pp_text (Paragraph cs) = pp_children cs <> text "\n\n"
pp_text (Heading level cs) = text (take (level + 1) (repeat '='))
                          <> char ' '
                          <> pp_children cs
                          <> char '\n'
pp_text _ = undefined

pp_child :: TextChild -> Doc
pp_child (Text s prop) = foldr applyProp (escape s) prop
-- Math mode probably needs to escape "$"
pp_child (Math src _ _) = pp_enclose (text "$$") (text src)
pp_child (Link href title) = text "link:" <> text href
                          <> char '[' <> pp_children title <> char ']'

pp_children :: [TextChild] -> Doc
pp_children = hcat . map pp_child

applyProp :: TextProp -> Doc -> Doc
applyProp Strong = pp_enclose (text "**")
applyProp Emphasis = pp_enclose (text "__")

-- There is probably some escaping necessary (which?)
escape :: String -> Doc
escape = text
