module Week40Exercise2 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

-- toList :: BinSearchTree a -> [a]
toList :: BinSearchTree a -> [a]
toList Empty = []
toList (Branch tl x tr) = toList tl ++ [x] ++ toList tr