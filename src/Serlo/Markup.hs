module Serlo.Markup
  (
    serloToMarkup
  , pluginToMarkup
  ) where

import Text.PrettyPrint.HughesPJ (render)
import Serlo.Markup.ModelToMarkup (pp_serlo, pp_plugin)

import Serlo.Model (SerloContent, SerloPlugin)

serloToMarkup :: SerloContent -> String
serloToMarkup = render . pp_serlo

pluginToMarkup :: SerloPlugin -> String
pluginToMarkup = render . pp_plugin
