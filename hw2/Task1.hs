module Test where

safeTail :: [a] -> Either [a] String
safeTail []     = Right "The list is empty"
safeTail (_:xs) = Left xs

safeInit :: [a] -> Either [a] String
safeInit [] = Right "The list is empty"
safeInit xs = Left $ init xs

strip :: [a] -> [a]
strip xs = case safeTail xs of
    Left tail -> case safeInit tail of
        Left result -> result
        Right message -> error "The list contain only one element"
    Right message -> error message