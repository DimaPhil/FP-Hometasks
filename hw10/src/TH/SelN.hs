module SelN where

import           Language.Haskell.TH (Exp (..), Pat (..), Q (..), newName)

selN :: Int -> Int -> Q Exp
selN n index = do
    x <- newName "x"
    return $ LamE [TupP $ replicate (index - 1) WildP ++
                   [VarP x] ++
                   replicate (n - index) WildP] (VarE x)
