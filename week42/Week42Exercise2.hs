module Week42Exercise2 where

import Data.List (reverse)
import Data.Bool (bool)
import Data.Function (on)

-- a) reverseWords :: String -> String (punktløs)
-- Reverserer hvert ord i en streng, men beholder rekkefølgen på ordene.
-- Funksjonsflyten:
-- words (splitter strengen i ord)
-- map reverse (reverserer hvert ord)
-- unwords (setter sammen ordene igjen til en streng)
reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- b) sumIsEven :: Integer -> Integer -> Bool (punktløs)
-- Sjekker om summen av de to heltallene er et partall.
-- Funksjonsflyten (ved bruk av `curry` og `uncurry` som tips):
-- uncurry (+) : gjør (+) om fra curry-form (a -> b -> c) til uncurry-form ((a, b) -> c).
-- (uncurry (+)) :: (Integer, Integer) -> Integer
-- uncurry (+) appliseres til de to inputverdiene m og n *etter* de er slått sammen til et tuppel (n, m)
-- (\(m, n) -> (m + n)) (m, n)
-- (`even` .) : appliserer `even` på resultatet av uncurry (+).
-- `even` :: Integer -> Bool
-- (\x -> even x) (m + n)
-- `curry` gjenoppretter den originale funksjonssignaturen (Integer -> Integer -> Bool).

sumIsEven :: Integer -> Integer -> Bool
sumIsEven = curry (even . uncurry (+))
