module Instances where

import           Prelude hiding (Either, Left, Right)
import           Tree    (Tree (Leaf, Node))

newtype Identity a = Identity { runIdentity :: a }
data Either a b    = Left a | Right b
newtype Const a b  = Const { getConst :: a }
data Pair a b      = Pair a b

instance Functor Identity where
    fmap f m = Identity $ f (runIdentity m)

instance Functor (Either a) where
    fmap f (Left v)  = Left v
    fmap f (Right v) = Right $ f v

instance Functor Tree where
    fmap _ Leaf       = Leaf
    fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)

instance Functor (Const a) where
    fmap _ (Const v) = Const v

instance Functor (Pair a) where
    fmap f (Pair x y) = Pair x (f y)

instance Applicative Identity where
    pure               = Identity
    (Identity f) <*> m = fmap f m

instance Applicative (Either a) where
    pure            = Right
    (Left f) <*> _  = Left f
    (Right f) <*> m = fmap f m

instance Applicative Tree where
    pure x     = Node x Leaf Leaf
    Leaf <*> _ = Leaf
    (Node f _ _) <*> node = fmap f node

instance Monoid m => Applicative (Const m) where
    pure _              = Const mempty
    Const f <*> Const v = Const $ f `mappend` v

instance Monoid a => Applicative (Pair a) where
    pure = Pair mempty
    (Pair x f) <*> (Pair y z) = Pair (x `mappend` y) (f z)

instance Foldable Identity where
    foldr f identity (Identity x) = f x identity

instance Foldable (Either a) where
    foldr _ identity (Left _) = identity
    foldr f identity (Right x) = f x identity

instance Foldable (Const m) where
    foldr _ identity _ = identity

instance Foldable (Pair a) where
    foldr f identity (Pair _ y) = f y identity

instance Traversable Identity where
    traverse f (Identity x) = pure Identity <*> f x

instance Traversable (Either a) where
    traverse _ (Left x) = pure $ Left x

instance Traversable Tree where
    traverse f Leaf = pure Leaf
    traverse f (Node v l r) = pure Node <*> f v <*> traverse f l <*> traverse f r

instance Traversable (Const m) where
    traverse _ (Const x) = pure $ Const x

instance Traversable (Pair a) where
    traverse f (Pair x y) = pure (Pair x) <*> f y
