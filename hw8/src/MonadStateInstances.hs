{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module MonadStateInstances where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Control.Monad.Trans.State  (StateT (..))

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

instance (Monad m) => MonadState s (StateT s m) where
    get = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

instance (MonadState s m) => MonadState s (MaybeT m) where
    get = lift get
    put = lift . put

instance (MonadState s m) => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put
