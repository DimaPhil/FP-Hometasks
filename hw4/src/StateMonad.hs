{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module StateMonad where

data State s a = State { runState :: s -> (a, s) }

instance Applicative (State s) => Monad (State s) where
    return a    = State $ \s -> (a, s)
    state >>= f = State $ \s -> let (a, newState) = runState state s
                                in runState (f a) newState
