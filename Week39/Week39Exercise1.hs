module Week39Exercise1 where

mySplitAt :: Integer -> [a] -> ([a], [a])
mySplitAt _ [] = ([], [])
mySplitAt n list@(x:xs)
  | n <= 0 = ([], list)
  | otherwise =
    let (left, right) = mySplitAt (n - 1) xs
    in (x : left, right)