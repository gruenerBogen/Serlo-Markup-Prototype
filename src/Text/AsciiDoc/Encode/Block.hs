module Text.AsciiDoc.Encode.Block
  (
    encodeBlock
  , encodeBlocks
  ) where

import Control.Monad.Trans.State ( State
                                 , evalState
                                 , get
                                 , put
                                 )

import Text.Read (readMaybe)
import Text.PrettyPrint.HughesPJ hiding ((<>))

import Text.AsciiDoc.Types.Generic ( Attributes(..)
                                   , attributesEmpty
                                   , lookupNamed
                                   )
import Text.AsciiDoc.Types.Block
import Text.AsciiDoc.Types.Text (FormattedText)
import Text.AsciiDoc.Encode.Text (encodeFormattedText)

encodeBlock :: Block -> Doc
encodeBlock = (flip evalState) initState . blockEncoder

encodeBlocks :: [Block] -> Doc
encodeBlocks = (flip evalState) initState . blocksEncoder

--------------------------------------------------------------------------------

data EncoderState = EncoderState { depth :: Int
                                 , sectionLevel :: Int
                                 }

initState :: EncoderState
initState = EncoderState { depth = 0
                         , sectionLevel = 0
                         }

type Encoder a = State EncoderState a

type BlockEncoder = Block -> (Encoder Doc)

blockEncoder :: BlockEncoder
blockEncoder b =
  case context b of
    "section" -> sectionEncoder b
    _ -> genericBlockEncoder b

sectionEncoder :: BlockEncoder
sectionEncoder b = do
  s <- get
  let level = (maybe ((sectionLevel s)+1) id $ readMaybe =<< (lookupNamed "level" $ attributes b))
  let t = maybe [] id (title b)
  put $ s { sectionLevel = level }
  c <- contentEncoder empty b
  put s
  return $ text (take (level + 1) $ repeat '=')
        <> char ' '
    <> encodeFormattedText t
    -- This \n should not be part of the title, but currently
    -- it is. Hence it is currently commented out.
    -- <> char '\n'
    <> c

genericBlockEncoder :: BlockEncoder
genericBlockEncoder b = do
  s <- get
  let d = depth s
  let sep = take (d+2) $ repeat '-'
  let sepDoc = if (null sep)
               then empty
               else (text sep)
  put s { depth = (d+2) }
  c <- contentEncoder sepDoc b
  put s
  return $ encodeBlockTitle (title b)
        <> maybeEncodeAttributes (attributes b)
        <> c

blocksEncoder :: [Block] -> Encoder Doc
blocksEncoder = fmap (hcat . punctuate (text "\n\n")) . mapM blockEncoder

-- TODO Ensure that the contentEncoder produces Docs which end with a new line
contentEncoder :: Doc -> Block -> Encoder Doc
contentEncoder sep b =
  case content b of
    Compount bs -> applySeparator sep <$> blocksEncoder bs
    Simple t -> return (encodeFormattedText t)
    Raw s -> return $ applySeparator sep (text s)
    Empty -> return empty

--------------------------------------------------------------------------------

applySeparator :: Doc -> Doc -> Doc
applySeparator s
  | isEmpty s = id
  | otherwise = sourround (s <> char '\n') (char '\n' <> s)

maybeEncodeAttributes :: Attributes -> Doc
maybeEncodeAttributes a = if (attributesEmpty $ a)
                          then empty
                          else encodeAttributes a <> char '\n'

encodeAttributes :: Attributes -> Doc
encodeAttributes (Attributes pos named) = brackets $ hcat $ punctuate (char ',') $ map text pos <> map encNamed named
  where encNamed (k, v) = text k <> char '=' <> text v

encodeBlockTitle :: Maybe FormattedText -> Doc
encodeBlockTitle = maybe empty (sourround (char '.') (char '\n') . encodeFormattedText)

sourround :: Doc -> Doc -> Doc -> Doc
sourround start end mid = start <> mid <> end
