{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module FromMonad where

import qualified Control.Monad as CM (join, (>=>))
import           MonadFish
import           MonadJoin

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g    = \x -> (f x >>= g)

instance Monad m => MonadJoin m where
    returnJoin = return
    join x     = x >>= id
