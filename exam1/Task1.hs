module First where

import Data.List (nub)

-- Leave in list only unique strings starting from given letter
leaveUniqueFromLetter :: [String] -> Char -> [String]
leaveUniqueFromLetter xs letter = nub $ filter (\ word -> head word == letter) xs

test1 = leaveUniqueFromLetter ["abacaba", "babacaba", "abacaba", "babacaba", "babacaba", "cccc", "ddd", "ax", "a239", "a239"] 'a'