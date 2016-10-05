{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Set where

import           Data.Foldable
import           Prelude       hiding (foldr)
import           Tree

class Set a b where
  emptySet :: (a b)
  isEmptySet :: (a b) -> Bool
  toList :: (a b) -> [b]
  find :: Ord b => (a b) -> b -> Bool
  insert :: Ord b => (a b) -> b -> (a b)
  delete :: Ord b => (a b) -> b -> (a b)
  next :: Ord b => (a b) -> b -> Maybe b
  fromList :: [b] -> (a b)

instance Ord a => Set Tree a where
  emptySet = Leaf

  isEmptySet Leaf = True
  isEmptySet _    = False

  toList tree = foldr (:) [] tree
  find = Tree.find
  insert = Tree.insert
  delete = Tree.delete
  next = Tree.next
  fromList = foldr (flip Tree.insert) Leaf
