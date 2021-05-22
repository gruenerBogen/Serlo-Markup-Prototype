module SerloSourcePP
  (
    pp_serlo
  ) where

import Text.PrettyPrint.HughesPJ hiding ((<>))
import Data.List (intersperse)

import Serlo.Model

pp_serlo :: SerloContent -> Doc
pp_serlo c = mconcat $ intersperse (text "\n\n") $ map pp_plugin c

pp_plugin :: SerloPlugin -> Doc
pp_plugin (Paragraph s) = text s
pp_plugin (Spoiler title content) = pp_genericPlugin "spoiler"
                                               [
                                                 ("title", text title)
                                               , ("rows", pp_serlo content)
                                               ]
pp_plugin (Image src maxWidth) = pp_image src maxWidth

pp_between :: Doc -> Doc -> Doc -> [Doc] -> Doc
pp_between start mid end cont = start
                             <> mconcat (intersperse mid cont)
                             <> end

pp_genericPlugin :: String -> [(String, Doc)] -> Doc
pp_genericPlugin name props = text "{{" <> text name <> char '\n'
                           <> mconcat (map pp_prop props)
                           <> text "}}"
  where pp_prop (n, d) = char '|' <> text n <> char '=' <> d <> char '\n'

pp_image :: String -> [] -> Doc
pp_image src props = text "[[" <> text src <> char '|' <> int maxWidth <> text "]]"
