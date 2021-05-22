module SerloSourceParse
  (
    fromString
  ) where

import Text.ParserCombinators.Parsec

import SerloModel

fromString = undefined

p_serlo :: CharParser SerloContent
p_serlo = many $ choose [
                          p_genericPlugin
                        , p_image
                        , p_paragraph
                        ]
       <* eof

p_genericPlugin :: CharParser (String, [(String, String)])
p_genericPlugin = between "{{" "}}" $ do
    title <- p_title
    args <- many p_arg
    return (title, args)
  where p_title = oneOf ['a'..'z']
        p_arg = p_title <* char '=' <*> noneOf "|}"

p_image :: CharParser SerloContent
p_image = between "[[" "]]" $ do
  src <- noneOf "|"
  maxWidth <- many digit
  return $ Image src maxWidth
