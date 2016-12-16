module FibCompile where

import           Language.Haskell.TH (Exp (..), Lit (..), Q (..))

fib :: Int -> Integer
fib n = let fibs = 1 : 1 : zipWith (+) fibs (tail fibs) in
            fibs !! n

ctFib :: Int -> Q Exp
ctFib n = return $ LitE (IntegerL $ fib n)
