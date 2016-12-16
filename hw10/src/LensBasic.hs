{-# LANGUAGE RankNTypes #-}

module LensBasic where

import           Control.Lens (Const (..), Identity (..), getConst, runIdentity)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

set :: Lens' s a -> a -> s -> s
set lens value state = runIdentity $ lens (\_ -> Identity value) state

view :: Lens' s a -> s -> a
view lens state = getConst $ lens (\x -> Const x) state

over :: Lens' s a -> (a -> a) -> s -> s
over lens f state = runIdentity $ lens (\x -> Identity $ f x) state
