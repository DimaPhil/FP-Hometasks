module Main where

import Lib
import ApplyingFunctions

duplicate :: Int -> [Int]
duplicate x = [x, x]

threeTimes :: Int -> [Int]
threeTimes x = [x, x, x]

increaseByTwo :: Int -> [Int]
increaseByTwo x = [x + 2]

makePair :: a -> (a, a)
makePair x = (x, x)

makePair2 :: Monoid a => a -> (a, a)
makePair2 x = (x, x `mappend` x)

main :: IO ()
main = print "running tests:" >>
       print "Task 1: " >>
       print (show $ apply [duplicate, threeTimes] 2) >>
       print (show $ apply [threeTimes, duplicate] 100) >>
       print (show $ apply [threeTimes, increaseByTwo, duplicate] 100) >>
       print (show $ apply [makePair, makePair2] "abc") >>
       print (show $ apply [makePair2, makePair] "abc") >>
       print (show $ apply [makePair, makePair2] [1, 2, 3]) >>
       print (show $ apply [makePair2, makePair] [1, 2, 3]) >>
       print "Task 2: " >> 
       return ()
