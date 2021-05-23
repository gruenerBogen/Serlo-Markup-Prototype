module Serlo.Markup.ModelToMarkup
  (
    pp_serlo
  , pp_plugin
  ) where

import Text.PrettyPrint.HughesPJ hiding ((<>))
import Data.List (intersperse)

import Serlo.Internal.Model ( SerloContent
                            , SerloPlugin(..))
import Serlo.Markup.TextToMarkup (pp_text)

pp_serlo :: SerloContent -> Doc
pp_serlo = pp_plugin

pp_plugin :: SerloPlugin -> Doc
pp_plugin (RichText ts) = hcat $ map pp_text ts
pp_plugin _ = undefined
