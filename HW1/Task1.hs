module Test where

parseInt :: String -> Int
parseInt number@('+':'-':xs) = error $ number ++ " is not a correct number"
parseInt ('+':xs) = read xs :: Int
parseInt xs = read xs :: Int

stringSum :: String -> Int
stringSum s = sum (map parseInt (words s))
