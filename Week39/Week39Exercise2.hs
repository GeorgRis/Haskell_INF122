module Week39Exercise2 where

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (Nothing : xs) = removeNothing xs
removeNothing ((Just x) : xs) = x : removeNothing xs