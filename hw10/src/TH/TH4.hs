{-# LANGUAGE TemplateHaskell #-}

module TH4 where

import           FibCompile (ctFib)

th4 :: IO ()
th4 = print $(ctFib 50000)
