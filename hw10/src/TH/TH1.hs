{-# LANGUAGE TemplateHaskell #-}

module TH1 where

import           SelN (selN)

th1 :: IO ()
th1 = do
    print $ $(selN 4 1) ("hello", 1, [4,3], 2)
    print $ $(selN 4 3) ("hello", 1, [4,3], 2)
