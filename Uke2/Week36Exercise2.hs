module Week36Exercise2 where

halfPalindrome :: String -> Maybe String
halfPalindrome s
  | s == reverse s = Just (take (length s `div` 2) s)
  | otherwise      = Nothing

decomposePalindrome :: String -> Maybe (String, Maybe Char)
decomposePalindrome s
  | s == reverse s = Just (half, middle)
  | otherwise      = Nothing
  where
    n = length s
    half = take (n `div` 2) s
    middle = if odd n then Just (s !! (n `div` 2)) else Nothing

createPalindrome :: String -> Maybe Char -> String
createPalindrome s middle = s ++ maybe "" (\c -> [c]) middle ++ reverse s