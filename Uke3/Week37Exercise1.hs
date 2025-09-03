module Week37Exercise1 where

-- Funksjonen equalCubeSum finner alle tupler (a,b,c,d)
-- hvor a^3 + b^3 = c^3 + d^3 og a,b,c,d <= n
-- Den bruker list comprehension for å generere og sjekke alle kombinasjoner
equalCubeSum :: Integer -> [(Integer, Integer, Integer, Integer)]
equalCubeSum n = [(a, b, c, d) | a <- [1..n],
                                  b <- [1..n],
                                  c <- [1..n],
                                  d <- [1..n],
                                  a^3 + b^3 == c^3 + d^3,
                                  a < b,
                                  c < d]