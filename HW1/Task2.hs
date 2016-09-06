module Test where

zipN :: ([a] -> a) -> [[a]] -> [a]
zipN f xs
  | length (head xs) == 0 = []
  | otherwise             = (f $ map head xs) : zipN f (map tail xs)