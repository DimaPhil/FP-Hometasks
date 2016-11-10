{-# LANGUAGE RankNTypes #-}

module ApplyingFunctions where

apply :: Monad m => [a -> m a] -> a -> m a
apply functions e = foldr (=<<) (return e) functions
