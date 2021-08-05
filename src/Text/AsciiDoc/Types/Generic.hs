module Text.AsciiDoc.Types.Generic
  (
    NamedAttributes
  , PositionalAttributes
  , Attributes(..)
  , lookupPositional
  , lookupNamed
  , attributesEmpty
  , emptyAttributes
  , fromList
  , fromAList
  ) where

type NamedAttributes = [(String, String)]
type PositionalAttributes = [String]

data Attributes = Attributes PositionalAttributes NamedAttributes
                deriving (Eq, Show)

lookupPositional :: Int -> Attributes -> Maybe String
lookupPositional n (Attributes ps _) = nth n ps

lookupNamed :: String -> Attributes -> Maybe String
lookupNamed s (Attributes _ ns) = lookup s ns

attributesEmpty :: Attributes -> Bool
attributesEmpty (Attributes [] []) = True
attributesEmpty _ = False

emptyAttributes :: Attributes
emptyAttributes = Attributes [] []

listToEither :: [Either a b] -> Either a [b]
listToEither [] = Right []
listToEither ((Left a):_) = Left a
listToEither ((Right b):xs) = let r = listToEither xs
                              in case r of
                                   Left a -> Left a
                                   Right bs -> Right (b:bs)

nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (x:_) = Just x
nth n (_:xs) = nth (n-1) xs

instance Semigroup Attributes where
  (Attributes p n) <> (Attributes p' n') = Attributes (p ++ p') (n ++ n')

instance Monoid Attributes where
  mempty = Attributes [] []

fromList :: [String] -> Attributes
fromList ps = Attributes ps []

fromAList :: [(String, String)] -> Attributes
fromAList ns = Attributes [] ns
