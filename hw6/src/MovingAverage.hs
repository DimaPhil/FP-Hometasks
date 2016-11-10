module MovingAverage where

import           Control.Monad.State (State (), evalState, get, gets, put)

findAverage :: Int -> Double -> State [Double] Double
findAverage n x = get >>= \es -> let nx = es ++ [x] in
                  put ((reverse . take n . reverse) nx) >>
                  gets (\v -> sum v / fromIntegral (length v))

moving :: Int -> [Double] -> [Double]
moving n xs = evalState (mapM (findAverage n) xs) []
