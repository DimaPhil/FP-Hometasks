module Second where

import Data.Char (isDigit)

-- Calculate sum of all numbers in string (string may contain any letters)
-- Example: stringSum "aba3ca ba12" = 15
stringSum :: String -> Int
stringSum [] = 0
stringSum [x]
  | isDigit x = read [x]
  | otherwise = 0
stringSum (x:y:xs)
  | x == '-' && isDigit y = stringSum remaining - value
  | x == '+' && isDigit y = stringSum remaining + value
  | isDigit x             = stringSum fremaining + fvalue
  | otherwise             = stringSum suffix
  where
    suffix = y:xs
    (fvalue, fremaining) = head (reads (x:suffix) :: [(Int, String)])
    (value, remaining) = head (reads suffix :: [(Int, String)])

test1 = stringSum "239 ccccccc212cccc lajsdlj-1lksajldkjlalk+-132lkasjd    \n\n\n\n\t\t\t\r-239"
test2 = stringSum "+++++++++++++---------1"