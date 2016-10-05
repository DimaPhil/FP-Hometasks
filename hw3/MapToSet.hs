{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Map where

import           Data.Foldable
import           Prelude       hiding (foldr)
import           Tree

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

  mtoList tree = foldr (:) [] (tmap makePair tree)
  mfind tree k = case Tree.findVertex tree (Pair k undefined) of
    Leaf       -> Nothing
    Node x l r -> Just $ second x
  minsert tree k v = Tree.insert tree (Pair k v)
  mdelete tree k = Tree.delete tree (Pair k undefined)
  mnext tree k = Tree.next tree (Pair k undefined) >>= (Just . makePair)
  mfromList = foldr (\(x, y) -> flip Tree.insert (Pair x y)) Leaf

class Set a b where
  emptySet :: a (Pair b Bool)
  isEmptySet :: a (Pair b Bool) -> Bool
  toList :: a (Pair b Bool) -> [b]
  find :: Ord b => a (Pair b Bool) -> b -> Bool
  insert :: Ord b => a (Pair b Bool) -> b -> a (Pair b Bool)
  delete :: Ord b => a (Pair b Bool) -> b -> a (Pair b Bool)
  next :: Ord b => a (Pair b Bool) -> b -> Maybe b
  fromList :: [b] -> a (Pair b Bool)

instance Ord k => Set Tree k where
    emptySet = Leaf

    isEmptySet Leaf = True
    isEmptySet _    = False

    toList = foldr (:) [] . tmap first
    find tree k = case mfind tree k of
      Nothing -> False
      Just _  -> True
    insert tree k = minsert tree k True
    delete = mdelete
    next tree k = mnext tree k >>= (Just . fst)
    fromList = foldr (\x -> flip Tree.insert (Pair x True)) Leaf
