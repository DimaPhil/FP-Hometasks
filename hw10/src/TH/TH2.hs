{-# LANGUAGE TemplateHaskell #-}

module TH2 where

import           EnvShow (envTemplate)

th2 :: IO ()
th2 = print $ "TH_ENV=" ++ $(envTemplate)
