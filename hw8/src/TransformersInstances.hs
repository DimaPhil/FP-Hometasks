{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module TransformersInstances where

import           Control.Monad             (fmap)
import           Control.Monad.Trans.Class (MonadTrans (), lift)
import           Data.Monoid               ((<>))

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Applicative (StateT s m), Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= f  = StateT $ \s -> do
        (a, s') <- runStateT m s
        runStateT (f a) s'

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        result <- m
        return (result, s)

instance (Applicative (WriterT w m), Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    m >>= f = WriterT $ do
        (a, w) <- runWriterT m
        (b, w') <- runWriterT (f a)
        return (b, w <> w')

instance (Monoid w) => MonadTrans (WriterT w) where
    lift m = WriterT $ do
        result <- m
        return (result, mempty)

instance (Applicative (EitherT e m), Monad m) => Monad (EitherT e m) where
    return a = EitherT $ return (Right a)
    m >>= f = EitherT $ do
        result <- runEitherT m
        case result of
            Left  x -> return (Left x)
            Right x -> runEitherT (f x)

instance MonadTrans (EitherT e) where
    lift = EitherT . fmap return
