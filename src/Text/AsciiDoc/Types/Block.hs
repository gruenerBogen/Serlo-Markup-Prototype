module Text.AsciiDoc.Types.Block where

import Text.AsciiDoc.Types.Generic ( Attributes
                                   , lookupNamed
                                   , emptyAttributes
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
