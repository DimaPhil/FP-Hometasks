{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Set where

import           Data.Foldable
import           Prelude       hiding (foldr)
import           Tree

class Set a b where
  emptySet :: a b
  isEmptySet :: a b -> Bool
  toList :: a b -> [b]
  find :: Ord b => a b -> b -> Bool
  insert :: Ord b => a b -> b -> a b
  delete :: Ord b => a b -> b -> a b
  next :: Ord b => a b -> b -> Maybe b
  fromList :: [b] -> a b

instance Ord a => Set Tree a where
  emptySet = Leaf

  isEmptySet Leaf = True
  isEmptySet _    = False

  toList = foldr (:) []
  find = Tree.find
  insert = Tree.insert
  delete = Tree.delete
  next = Tree.next
  fromList = foldr (flip Tree.insert) Leaf

data Pair f s = Pair f s deriving Show

instance Eq f => Eq (Pair f s) where
  (Pair x1 _) == (Pair x2 _) = x1 == x2

instance Ord f => Ord (Pair f s) where
  (Pair x1 _) `compare` (Pair x2 _) = x1 `compare` x2

makePair :: Pair f s -> (f, s)
makePair (Pair x y) = (x, y)

first :: Pair f s -> f
first (Pair x _) = x

second :: Pair f s -> s
second (Pair _ y) = y

class Map a k v where
  emptyMap :: a (Pair k v)
  isEmptyMap :: a (Pair k v) -> Bool
  mtoList :: a (Pair k v) -> [(k, v)]
  mfind :: Ord k => a (Pair k v) -> k -> Maybe v
  minsert :: Ord k => a (Pair k v) -> k -> v -> a (Pair k v)
  mdelete :: Ord k => a (Pair k v) -> k -> a (Pair k v)
  mnext :: Ord k => a (Pair k v) -> k -> Maybe (k, v)
  mfromList :: [(k, v)] -> a (Pair k v)

instance Ord k => Map Tree k v where
  emptyMap = Leaf

  isEmptyMap Leaf = True
  isEmptyMap _    = False

  mtoList = foldr (:) [] . tmap makePair
  mfind tree k = case Tree.findVertex tree (Pair k undefined) of
    Leaf       -> Nothing
    Node x l r -> Just $ second x
  minsert tree k v = Set.insert tree (Pair k v)
  mdelete tree k = Set.delete tree (Pair k undefined)
  mnext tree k = Set.next tree (Pair k undefined) >>= (Just . makePair)
  mfromList = foldr (\(x, y) -> flip Set.insert (Pair x y)) Leaf
