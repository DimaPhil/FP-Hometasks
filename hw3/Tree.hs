module Tree where

import           Data.Foldable hiding (find, maximum, toList)
import           Data.Monoid
import           Prelude       hiding (foldr)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

instance Ord a => Monoid (Tree a) where
  mempty = Leaf
  mappend Leaf r = r
  mappend l Leaf = l
  mappend v1@Node{} (Node y l2 r2) = mappend (insert v1 y) (merge l2 r2)

instance Foldable Tree where
  foldr _ identity Leaf = identity
  foldr f identity (Node x l r) = foldr f (f x (foldr f identity r)) l

tmap :: (a -> b) -> Tree a -> Tree b
tmap _ Leaf         = Leaf
tmap f (Node x l r) = Node (f x) (tmap f l) (tmap f r)

findVertex :: Ord a => Tree a -> a -> Tree a
findVertex Leaf _ = Leaf
findVertex v@(Node x l r) key
  | key == x  = v
  | key < x   = findVertex l key
  | otherwise = findVertex r key

find :: Ord a => Tree a -> a -> Bool
find tree key = case findVertex tree key of
  Leaf -> False
  _    -> True

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf key = Node key Leaf Leaf
insert (Node x l r) key
  | key == x  = Node x l r
  | key < x   = Node x (insert l key) r
  | otherwise = Node x l (insert r key)

findMax :: Ord a => Tree a -> a
findMax Leaf               = undefined
findMax (Node x Leaf Leaf) = x
findMax (Node x Leaf r)    = max x $ findMax r
findMax (Node x l Leaf)    = max x $ findMax l
findMax (Node x l r)       = maximum [x, findMax l, findMax r]

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Leaf Leaf = Leaf
merge Leaf r    = r
merge l Leaf    = l
merge l r       = Node best newl r
  where
    best = findMax l
    newl = delete l best

delete :: Ord a => Tree a -> a -> Tree a
delete Leaf _ = Leaf
delete (Node x l r) key
  | key == x  = merge l r
  | key < x   = Node x (delete l key) r
  | otherwise = Node x l (delete r key)

next :: Ord a => Tree a -> a -> Maybe a
next Leaf _   = Nothing
next (Node x l r) k
  | x > k     = case next l k of
    Nothing -> Just x
    Just y  -> Just y
  | otherwise = next r k
