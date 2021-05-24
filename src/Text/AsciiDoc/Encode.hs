module Text.AsciiDoc.Encode () where

import Text.PrettyPrint.HughesPJ hiding ((<>))

import Text.AsciiDoc.Types.Header
import Text.AsciiDoc.Types.Generic

encodeHeader :: Header -> Doc
encodeHeader (Header title authorRev attrs) = maybe empty encodeTitle title
                                           $$ maybe empty encodeAuthorRevision authorRev
                                           $$ encodeDocumentAttributes attrs

encodeTitle = encodeSectionHeading 0

encodeSectionHeading :: Int -> Title -> Doc
encodeSectionHeading n title = text (take (n + 1) $ repeat '=') <> char ' ' <> encodeText title

encodeAuthorRevision :: AuthorRevision -> Doc
encodeAuthorRevision (AuthorRevision as rev) = hcat (punctuate (text "; ") (map encodeAuthor as))
                                            $$ maybe empty encodeRevision rev

encodeAuthor :: Author -> Doc
encodeAuthor (Author name mail) = text name <+> maybe empty encodeMail mail
  where encodeMail m = char '<' <> text m <> char '>'

encodeRevision :: Revision -> Doc
encodeRevision (Revision nr Nothing Nothing) = char 'v' <> text nr
encodeRevision (Revision nr date rmk) = text nr
                                     <> maybe empty (\d -> text ", " <> text d) date
                                     <> maybe empty (\r -> text ": " <> text r) rmk

encodeDocumentAttributes :: Attributes -> Doc
encodeDocumentAttributes = vcat . map encodeAttr
  where encodeAttr (k,v) = char ':' <> text k <> text ": " <> text v

encodeText :: Title -> Doc
encodeText = text
