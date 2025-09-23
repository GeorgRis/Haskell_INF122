module Week40Exercise0 where


data Palindrome
  = EvenPal String           -- for partallslengde
  | OddPal String Char       -- for oddetallslengde
  deriving (Eq, Show)

palindrome :: String -> Maybe Palindrome
palindrome s
  | s == reverse s =
      let n = length s
          half = take (n `div` 2) s
       in if even n
             then Just (EvenPal half)
             else Just (OddPal half (s !! (n `div` 2)))
  | otherwise = Nothing

toString :: Palindrome -> String
toString (EvenPal half) = half ++ reverse half
toString (OddPal half mid) = half ++ [mid] ++ reverse half
