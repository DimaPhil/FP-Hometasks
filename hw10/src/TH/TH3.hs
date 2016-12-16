{-# LANGUAGE TemplateHaskell #-}

module TH3 where

import           Data.List  (intercalate)
import           PrettyShow (A (..), MyData (..), prettyShow)

prettyShow ''MyData

th3 :: IO ()
th3 = print MyData { foo = A 3, bar = 5 }
