module Week37Exercise2 where

import Data.List (unwords, reverse)
import Data.Maybe (fromMaybe)

{- Takes the numbers used in the words for the integers 0 to 999 999 999 and
returns their corresponding words -}
atomicNumberToWord :: Integer -> String
atomicNumberToWord 0 = "zero"
atomicNumberToWord 1 = "one"
atomicNumberToWord 2 = "two"
atomicNumberToWord 3 = "three"
atomicNumberToWord 4 = "four"
atomicNumberToWord 5 = "five"
atomicNumberToWord 6 = "six"
atomicNumberToWord 7 = "seven"
atomicNumberToWord 8 = "eight"
atomicNumberToWord 9 = "nine"
atomicNumberToWord 10 = "ten"
atomicNumberToWord 11 = "eleven"
atomicNumberToWord 12 = "twelve"
atomicNumberToWord 13 = "thirteen"
atomicNumberToWord 14 = "fourteen"
atomicNumberToWord 15 = "fifteen"
atomicNumberToWord 16 = "sixteen"
atomicNumberToWord 17 = "seventeen"
atomicNumberToWord 18 = "eighteen"
atomicNumberToWord 19 = "nineteen"
atomicNumberToWord 20 = "twenty"
atomicNumberToWord 30 = "thirty"
atomicNumberToWord 40 = "forty"
atomicNumberToWord 50 = "fifty"
atomicNumberToWord 60 = "sixty"
atomicNumberToWord 70 = "seventy"
atomicNumberToWord 80 = "eighty"
atomicNumberToWord 90 = "ninety"
atomicNumberToWord 100 = "hundred"
atomicNumberToWord 1000 = "thousand"
atomicNumberToWord 1000000 = "million"
atomicNumberToWord _ =
  error "atomicNumberToWord: not a number used in the word for an integer from 0 to 999 999 999"

singleton :: a -> [a]
singleton x = [x]

{- Decomposes an integer into the digits it consists of in little endian
order (i.e. in ascending power of 10 going from left to right).
Example: digits 123 = [3,2,1]
 -}
digits :: Integer -> [Integer]
digits n = map (\c -> read [c] :: Integer) (reverse (show n))

{- Groups the elements of a list into sublists of n consecutive elements.
Example: chunksOf 3 [1,2,3,4,5,6,7,8] = [[1,2,3],[4,5,6],[7,8]] -}
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0 = error "chunksOf: cannot group into fewer than 1 elements"
  | otherwise =
    case splitAt n xs of
      ([], _) -> []
      (h, t) -> h : chunksOf n t

{- Takes a list of the (up to three) digits in a number from 0 to 999 in little
endian order and returns the list of the atomic numbers used in the
corresponding word (in the corresponding order, i.e. big endian).
Example: decomposeHundreds [2,3,5] = [5,100,30,2] -}
decomposeHundreds :: [Integer] -> [Integer]
decomposeHundreds =
  concat
    . reverse
    . filter ((/= 0) . head)
    . uncurry (zipWith ($))
    . createTagFunctions
  where
    createTagFunctions :: [Integer] -> ([Integer -> [Integer]], [Integer])
    createTagFunctions (n : 1 : xs) = ([singleton, (: [100])], (n + 10) : xs)
    createTagFunctions xs = ([singleton, singleton . (* 10), (: [100])], xs)

{- Takes a number and groups the digits into sublists of 3 consecutive digits in
little endian order -}
groupByThousands :: Integer -> [[Integer]]
groupByThousands n = chunksOf 3 (digits n)

{- Maps any sublist that contains only zeroes to Nothing and any other list xs
to Just xs -}
zeroToNothing :: [[Integer]] -> [Maybe [Integer]]
zeroToNothing xss = map (\xs -> if all (== 0) xs then Nothing else Just xs) xss

{- Takes a number from 0 to 999 999 999 and returns the decomposition into the
numbers used in the corresponding word -}
decomposeNumber :: Integer -> [Integer]
decomposeNumber 0 = [0]
decomposeNumber n
  | n < 0 || n >= 10 ^ 9 =
    error "decomposeNumber: cannot decompose number smaller than 0 or larger than 999 999 999"
  | otherwise =
    concat $
      reverse $
        zipWith ($) (fromMaybe [] : map (\k -> maybe [] (++ [1000 ^ k])) [1 ..]) $
          map (fmap decomposeHundreds) $
            zeroToNothing $ groupByThousands n

{- Takes a number from 0 to 999 999 999 and returns the word for it in English
-}
numberToWord :: Integer -> String
numberToWord = unwords . map atomicNumberToWord . decomposeNumber