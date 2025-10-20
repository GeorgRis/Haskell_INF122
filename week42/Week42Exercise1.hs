module Week42Exercise1 where

import Data.Either (either)

-- a) fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
-- Splitter en funksjon definert på Either a b til to funksjoner.
fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight f = (\x -> f (Left x), \y -> f (Right y))

-- b) either' :: (a -> c) -> (b -> c) -> Either a b -> c
-- En funksjon som bygger en Either-funksjon fra to andre funksjoner.
-- Dette er i praksis den standardfunksjonen 'either' fra Data.Either, men implementeres her.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g e = case e of
  Left x  -> f x
  Right y -> g y

-- c) toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
-- Splitter en funksjon med par som output til to funksjoner.
toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd f = (fst . f, snd . f)

-- d) pair :: (a -> b) -> (a -> c) -> a -> (b, c)
-- Setter sammen to funksjoner med samme domene til en funksjon som gir et tuppel ut.
pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f g x = (f x, g x)

