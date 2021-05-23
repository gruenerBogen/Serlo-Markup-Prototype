module Serlo.Markup.ToMarkup
  (
    pp_between
  , pp_enclose
  ) where

import Text.PrettyPrint.HughesPJ hiding ((<>))
import Data.List (intersperse)

pp_between :: Doc -> Doc -> Doc -> [Doc] -> Doc
pp_between start mid end cont = start
                             <> mconcat (intersperse mid cont)
                             <> end

pp_enclose :: Doc -> Doc -> Doc
pp_enclose enc = pp_between enc empty enc . (:[])
