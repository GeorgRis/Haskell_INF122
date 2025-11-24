module Week36Exercise1 where

f :: [Integer] -> Integer -> Bool
f xs n = sum xs > n

-- Øving
main :: IO()
main =
    putStr "Skriv setning"
    input <- getLine
    let wordCount = length (words input)
    putStrLN $ "Setningen bestod av " ++ wordCount


even :: [Int] -> [Int]
even xs = filter even xs

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs

unique :: (Eq a) => [a] -> Bool
unique [] = True
unique (x:xs) = not (elem x xs) && unique xs

unzip :: [(a,b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((a,b):xs) =
    let (as, bs) = unzip xs
    in (a:as, b:bs)

main :: IO()
main = do
    putStrLn "Hva heter du"
    name <- getLine
    rName = reverse name
    putStrLn "hei," ++ rName


sumOfSquares :: [Integer] -> Integer
sumOfSquares xs = sum [x^2 | x <- xs]
sumOfSquares = sum . map^2


duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

= (map snd . filter (odd . fst) . zip [0..]) "abcdefg"
= map snd (filter (odd . fst) (zip [0..] "abcdefg"))
= map snd (filter (odd . fst) [(0,'a'),(1,'b'),(2,'c'),(3,'d'),(4,'e'),(5,'f'),(6,'g')])
= map snd [(1,'b'),(3,'d'),(5,'f')]
= [snd (1,'b'), snd (3,'d'), snd(5,'f')]
= ['b', 'e', 'f']

product :: [Integer] -> Integer
product = foldr (*) 1



