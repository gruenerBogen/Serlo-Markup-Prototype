module Text.AsciiDoc.Types.Block where

import Text.AsciiDoc.Types.Generic ( Attributes(Attributes)
                                   , lookupNamed
                                   , emptyAttributes
                                   , fromAList
                                   )
import Text.AsciiDoc.Types.Text (FormattedText)

data Block = Block { context :: String
                   , attributes :: Attributes
                   , title :: Maybe FormattedText
                   , content :: BlockContent
                   } deriving (Eq, Show)

data BlockContent = Compount [Block]
                  | Simple FormattedText
                  -- Currently it is unclear what VerbatimText is.
                  -- | Verbatim VerbatimText
                  | Raw RawContent
                  | Empty
                  deriving (Eq, Show)

type RawContent = String

type Role = String

-- Some default blocks so that the compiler can check that everything is there
paragraph :: FormattedText -> Block
paragraph t = Block { context = "paragraph"
                    , attributes = emptyAttributes
                    , title = Nothing
                    , content = Simple t
                    }

section :: Int -> FormattedText -> [Block] -> Block
section level t c = Block { context = "section"
                          , attributes = Attributes [] [("level", show level)]
                          , title = Just t
                          , content = Compount c
                          }

blockMacroCall :: String -> Maybe String -> Attributes -> Block
blockMacroCall ctx target as = let targetAttr = fromAList $ maybe [] (\t -> [("target", t)]) target
                               in  Block { context = ctx
                                         , attributes = targetAttr <> as
                                         , title = Nothing
                                         , content = Empty
                                         }

-- Todo Add macro call with title
