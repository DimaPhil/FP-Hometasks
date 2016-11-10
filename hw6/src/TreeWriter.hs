module TreeWriter where

import           Control.Monad.Writer (Writer (), writer)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

findVertex :: (Show a, Ord a) => Tree a -> a -> Writer String (Tree a)
findVertex Leaf x = writer (Leaf, "Tree doesn't contain value " ++ show x)
findVertex v@(Node x l r) key
  | key == x  = writer (v, "key == " ++ show key ++ ", value is found")
  | key < x   = findVertex l key >>=
                \value -> writer (value,
                          "key=" ++ show x ++ " < " ++ show key ++ ", going to the left subtree")
  | otherwise = findVertex r key >>=
                \value -> writer (value,
                          "key=" ++ show x ++ " > " ++ show key ++ ", going to the right subtree")

find :: (Show a, Ord a) => Tree a -> a -> Writer String (Tree a)
find tree key = findVertex tree key >>= \value -> writer (value,
                                                  "trying to find " ++ show key)

insert :: (Show a, Ord a) => Tree a -> a -> Writer String (Tree a)
insert Leaf key = writer (Node key Leaf Leaf, "adding " ++ show key ++ " to the empty tree")
insert (Node x l r) key
  | key == x  = writer (Node x l r, "key " ++ show x ++ " already exists in tree, ignoring it")
  | key < x   = insert l key >>=
                \value -> writer (Node x value r,
                          "key=" ++ show x ++ " < " ++ show key ++ ", going to the left subtree")
  | otherwise = insert r key >>=
                \value -> writer (Node x l value,
                          "key=" ++ show x ++ " > " ++ show key ++ ", going to the right subtree")

findMax :: Ord a => Tree a -> Writer String a
findMax Leaf               = writer (undefined, "There is no max value in empty tree")
findMax (Node x _ Leaf) = writer (x, "Vertex has no right son, so it's value is maximum")
findMax (Node x _ r)    = findMax r >>= \vr -> writer (vr, "going to the right son")

merge :: (Show a, Ord a) => Tree a -> Tree a -> Writer String (Tree a)
merge Leaf Leaf = writer (Leaf, "Merging two empty trees")
merge Leaf r    = writer (r, "Merging empty tree and non-empty")
merge l Leaf    = writer (l, "Merging non-empty tree and empty")
merge l r       = findMax l >>= \best -> delete l best >>= \newl -> writer (Node best newl r,
                                                                    "Merging two non-empty trees")

delete :: (Show a, Ord a) => Tree a -> a -> Writer String (Tree a)
delete Leaf _ = writer (Leaf, "Deleting from empty tree")
delete (Node x l r) key
  | key == x  = merge l r >>= \value -> writer (value,
                                        "Found key to delete, merging its left and right son")
  | key < x   = delete l key >>= \value -> writer (Node x value r,
                                           "key=" ++ show x ++ " < " ++ show key ++
                                           ", going to the left tree")
  | otherwise = delete r key >>= \value -> writer (Node x l value,
                                           "key=" ++ show x ++ " > " ++ show key ++
                                           ", going to the right tree")

upgradeMaybe :: Maybe a -> a -> Maybe a
upgradeMaybe Nothing x = Just x
upgradeMaybe (Just x) _ = Just x

next :: Ord a => Tree a -> a -> Writer String (Maybe a)
next Leaf _   = writer (Nothing, "Can't find next value in empty tree")
next (Node x l r) k
  | x > k     = next l k >>= \value -> writer (upgradeMaybe value x,
                                       "x > k, trying to find better answer in the left subtree")
  | otherwise = next r k >>= \value -> writer (value,
                                       "x <= k, trying to find better answer in the right subtree")
