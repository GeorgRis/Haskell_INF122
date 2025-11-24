module Week43Exercise0 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

-- a) Lag en Foldable for BinSearchTree
-- foldr for et binært søketre må traversere treet i in-orden (venstre, rot, høyre)
-- for å sikre at elementene behandles i sortert rekkefølge.
instance Foldable BinSearchTree where
  -- foldr :: (a -> b -> b) -> b -> BinSearchTree a -> b
  foldr _ acc Empty = acc
  foldr f acc (Branch left root right) =
    -- Først foldes den høyre delen
    let acc' = foldr f acc right
    -- Deretter brukes rotverdien (f root acc')
    -- Til slutt foldes den venstre delen
    in  foldr f (f root acc') left

-- b) Lag funksjonen toList ved hjelp av foldr
-- Ved å bruke foldr med kons-operatoren (:) og en tom liste ([]),
-- omdannes treet til en liste. Takket være in-orden-traverseringen i foldr,
-- vil listen være sortert.
toList :: BinSearchTree a -> [a]
toList = foldr (:) []
