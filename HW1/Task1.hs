module Test where

parseInt :: String -> Int
parseInt ('+':xs) = read xs :: Int
parseInt xs = read xs :: Int

stringSum :: String -> Int
stringSum s = sum (map parseInt (words s))
