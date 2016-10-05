{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Map where

import           Data.Foldable
import           Prelude       hiding (foldr)
import           Set
import           Tree


class Map a k v where
  emptyMap :: (a (k, v))
  isEmptyMap :: (a (k, v)) -> Bool
  toList :: (a (k, v)) -> [(k, v)]
  find :: Ord k => (a (k, v)) -> k -> v -> Bool
  insert :: Ord k => (a (k, v)) -> k -> v -> (a (k, v))
  delete :: Ord k => (a (k, v)) -> k -> v -> (a (k, v))
  next :: Ord k => (a (k, v)) -> k -> v -> Maybe (k, v)
  fromList :: [(k, v)] -> (a (k, v))

instance (Ord k, Ord v) => Map Tree k v where
  emptyMap = Leaf

  isEmptyMap Leaf = True
  isEmptyMap _    = False

  toList tree = foldr (:) [] tree
  find tree k v = Set.find tree (k, v)
  insert tree k v = Set.insert tree (k, v)
  delete tree k v = Set.delete tree (k, v)
  next tree k v = Set.next tree (k, v)
  fromList = foldr (flip Tree.insert) Leaf
