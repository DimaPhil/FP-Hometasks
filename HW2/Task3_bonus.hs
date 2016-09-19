module Test where

import System.Random (randomR, mkStdGen)
import Control.Applicative

randInt :: Int -> Int -> Int
randInt from to = fst $ randomR (from, to) (mkStdGen 66)

data Tree a = Leaf | Node { value :: a, minv :: a, y :: Int, size :: Int, l :: Tree a, r :: Tree a } deriving Show

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

getSize :: Tree a -> Int
getSize Leaf = 0
getSize t    = t & size

findNewMin :: Ord a => Tree a -> a
findNewMin Leaf                            = undefined
findNewMin t@(Node { l = Leaf, r = Leaf }) = t & value
findNewMin t@(Node { l = Leaf })           = min (t & value) (t & r & minv)
findNewMin t@(Node { r = Leaf })           = min (t & value) (t & l & minv)
findNewMin t                               = minimum [t & value, t & l & minv, t & r & minv]

recalc :: Ord a => Tree a -> Tree a
recalc Leaf = undefined
recalc t    = t { minv = newMin, size = newSize } where
  newMin = findNewMin t
  newSize = getSize (t & l) + getSize (t & r) + 1

split :: Ord a => Tree a -> Int -> (Tree a, Tree a)
split Leaf _ = (Leaf, Leaf)
split t k = let
    (t1, t2) = split (t & l) k
    newL = recalc $ t { l = t2 }
    (t3, t4) = split (t & r) (k - getSize (t & l) - 1)
    newR = recalc $ t { r = t3 }
  in if getSize (t & l) >= k then (t1, newL) else (newR, t4)

merge :: Ord a => Tree a -> Tree a -> Tree a
merge l Leaf = l
merge Leaf r = r
merge lt rt = let
    t1 = merge (lt & r) rt
    newt1 = recalc $ lt { r = t1 }
    t2 = merge lt (rt & l)
    newt2 = recalc $ rt { l = t2 }
  in if (lt & y) < (rt & y) then newt1 else newt2

pushBack :: Ord a => Tree a -> a -> Tree a
pushBack root x = merge root newv where 
  ry = randInt 0 (10 ^ 9)
  newv = Node { value = x, minv = x, y = ry, size = 1, l = Leaf, r = Leaf }

getMin :: Ord a => Tree a -> Int -> Int -> a
getMin root l r = answer where
  (t1, _) = split root r
  (_, t2) = split t1 (l - 1)
  answer  = t2 & minv