module Text.AsciiDoc.Types.Header where

import Text.AsciiDoc.Types.Generic (Attributes)

data Header = Header (Maybe Title) (Maybe AuthorRevision) Attributes

-- TODO: Somehow ensure that at least one author is present
data AuthorRevision = AuthorRevision [Author] (Maybe Revision)

data Author = Author Name (Maybe Mail)

type Name = String
type Mail = String

data Revision = Revision RevNr (Maybe RevDate) (Maybe RevRemark)

type RevNr = String
-- Maybe this should be some date type?
type RevDate = String
type RevRemark = String

type Title = String
