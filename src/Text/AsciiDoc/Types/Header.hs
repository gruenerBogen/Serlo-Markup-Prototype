module Text.AsciiDoc.Types.Header where

import Text.AsciiDoc.Types.Generic (NamedAttributes)

data Header = Header (Maybe Title) (Maybe AuthorRevision) NamedAttributes
            deriving (Eq, Show)

-- TODO: Somehow ensure that at least one author is present
data AuthorRevision = AuthorRevision [Author] (Maybe Revision)
                     deriving (Eq, Show)

data Author = Author AuthorName (Maybe Mail)
            deriving (Eq, Show)

type AuthorName = String
type Mail = String

data Revision = Revision RevNr (Maybe RevDate) (Maybe RevRemark)
              deriving (Eq, Show)

type RevNr = String
-- Maybe this should be some date type?
type RevDate = String
type RevRemark = String

type Title = String
