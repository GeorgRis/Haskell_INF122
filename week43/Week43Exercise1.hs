module Week43Exercise1 where

import Data.List (sum)

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Show)

-- a) Lag en instans av Functor for RoseTree
-- Funksjonen fmap appliserer en funksjon f til verdien i roten (a)
-- og rekursivt til alle subtrærne i listen over barn ([RoseTree a]).
instance Functor RoseTree where
  -- fmap :: (a -> b) -> RoseTree a -> RoseTree b
  fmap f (Branch x subtrees) =
    Branch (f x) (fmap (fmap f) subtrees)
    -- eller: Branch (f x) (map (fmap f) subtrees)

-- b) Lag funksjonen sumNodes ved hjelp av fmap
-- sumNodes tar en RoseTree hvor hver node inneholder en [a], og bruker
-- fmap til å applisere funksjonen 'sum' (som summerer listen) til hver node.
sumNodes :: (Num a) => RoseTree [a] -> RoseTree a
sumNodes = fmap sum