{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module MonadJoin where

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a
