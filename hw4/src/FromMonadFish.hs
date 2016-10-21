{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module FromMonadFish where

import           MonadFish
import           MonadJoin

instance (Applicative m, MonadFish m) => Monad m where
    return  = returnFish
    (>>=)   = flip (id >=>)

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join       = id >=> id
