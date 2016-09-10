module Test where

zipN :: ([a] -> b) -> [[a]] -> [b]
zipN f xs
  | null $ head xs = []
  | otherwise      = (f $ map head xs) : zipN f (map tail xs)