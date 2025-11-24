module Week42Exercise0 where

-- Funksjonen applyFunctions appliserer tilsvarende funksjon på tilsvarende verdi,
-- og stopper når den korteste listen er slutt.
applyFunctions :: [a -> b] -> [a] -> [b]
applyFunctions = zipWith ($)