module Week37Exercise0 where

-- Tar et navn og en alder og returnerer en streng.
formatString :: String -> Integer -> String
formatString name age = name ++ " is " ++ show age ++ " years old"

-- Funksjonen namesAndAges tar en liste med navn og en liste med alder.
namesAndAges :: [String] -> [Integer] -> [String]
namesAndAges names ages =
  let
    -- Kombinerer navn og aldrer til en liste av tupler.
    zippedList = zip names ages
    -- Filtrerer listen, beholder bare de med alder <= 50.
    filteredList = filter (\(_, age) -> age <= 50) zippedList
    -- Mapper den filtrerte listen for å lage formatert streng for hvert par.
    result = map (\(name, age) -> formatString name age) filteredList
  in
    result