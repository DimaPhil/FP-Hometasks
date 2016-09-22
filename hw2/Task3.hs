module Test where

data Tree a = Leaf | Node a (Tree a) (Tree a)

find :: Ord a => Tree a -> a -> Bool
find Leaf key         = False
find (Node x l r) key = if x == key then True else (find l key || find r key)

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf key = Node key Leaf Leaf
insert tree@(Node x l r) key
  | key == x  = tree
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
delete Leaf key = Leaf
delete tree@(Node x l r) key
  | key == x  = merge l r
  | key < x   = Node x (delete l key) r
  | otherwise = Node x l (delete r key)

toList :: Tree a -> [a]
toList Leaf         = []
toList (Node x l r) = toList l ++ [x] ++ toList r

fromList :: Ord a => [a] -> Tree a
fromList []     = Leaf
fromList (x:xs) = Node x (fromList $ filter (< x) xs) (fromList $ filter (> x) xs)