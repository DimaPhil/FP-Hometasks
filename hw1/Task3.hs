module Test where

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge a@(x:xs) b@(y:ys) = if x < y then x : merge xs b else y : merge a ys

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge left right
  where 
    middle = length xs `div` 2
    left = mergeSort (take middle xs)
    right = mergeSort (drop middle xs)