module Main where

import InteractiveCmd (cmd)
import ExprParser (parseIO)

main :: IO ()
main = parseIO
