{-# LANGUAGE FlexibleContexts #-}

module DynamicArray where

import           Control.Monad.ST (ST, runST)
import           Data.Array.ST    (STArray, getBounds, getElems, newArray,
                                   newListArray, readArray, writeArray)
import           Data.Foldable    (forM_)
import           Data.STRef       (STRef (..), modifySTRef, newSTRef, readSTRef,
                                   writeSTRef)

data DynamicArray s a = DynamicArray
                        {
                          elements :: !(STRef s (STArray s Int a))
                        , size     :: !(STRef s Int)
                        }

getCapacity :: DynamicArray s a -> ST s Int
getCapacity array = do
    es <- readSTRef $ elements array
    (_, n) <- getBounds es
    return n

getSize :: DynamicArray s a -> ST s Int
getSize array = readSTRef $ size array

getElements :: DynamicArray s a -> ST s (STArray s Int a)
getElements array = readSTRef $ elements array

empty :: ST s (DynamicArray s a)
empty = do
    array <- newArray (0, 1) undefined
    nelements <- newSTRef array
    nsize <- newSTRef 0
    return DynamicArray { elements = nelements, size = nsize }

pushBack :: DynamicArray s a -> a -> ST s ()
pushBack array e = do
    nsize <- getSize array
    capacity <- getCapacity array
    modifySTRef (size array) (1 +)
    if capacity > nsize then
        setByIndex array nsize e
    else do
        curElements <- toList array
        nElements <- newListArray (0, capacity * 2) curElements
        writeSTRef (elements array) nElements
        setByIndex array nsize e

popBack :: DynamicArray s a -> ST s a
popBack array = do
    nsize <- getSize array
    x <- getByIndex array (nsize - 1)
    modifySTRef (size array) (flip (-) 1)
    return x

getByIndex :: DynamicArray s a -> Int -> ST s a
getByIndex array index = do
    es <- getElements array
    readArray es index

setByIndex :: DynamicArray s a -> Int -> a -> ST s ()
setByIndex array index e = do
    es <- getElements array
    writeArray es index e
    writeSTRef (elements array) es

fromList :: [a] -> ST s (DynamicArray s a)
fromList lst = do
    es <- newSTRef =<< newListArray (0, n + 1) lst
    nsize <- newSTRef n
    return DynamicArray { elements = es, size = nsize }
  where
    n = length lst

toList :: DynamicArray s a -> ST s [a]
toList array = do
    nsize <- getSize array
    curElements <- getElements array
    es <- getElems curElements
    return (take nsize es)

concat :: [Int] -> [Int] -> [Int]
concat a b = runST $ do
    a' <- fromList a
    b' <- fromList b
    size <- getSize b'
    forM_ [0..size - 1] $ \i -> do
        e <- getByIndex b' i
        pushBack a' e
    toList a'

sort :: [Int] -> [Int]
sort list = runST $ do
    let size = length list
    arr <- fromList list
    forM_ [1..size - 1] $ \i ->
        forM_ [i - 1, i - 2..0] $ \j -> do
            cur <- getByIndex arr j
            next <- getByIndex arr (j + 1)
            if cur > next then do
                setByIndex arr j next
                setByIndex arr (j + 1) cur
            else
                undefined
    toList arr

{-merge ::

mergeSort :: [Int] -> [Int]
mergeSort list = runST $ do
    let size = length list
    if size <= 1 then
        list
    else do
        let middle = size `div` 2
        let (left, right) = splitAt middle list
        sleft <- mergeSort left
        sright <- mergeSort right
        merge sleft sright
    toList arr -}

testPopBack :: [Int] -> [Int]
testPopBack l = runST $ do
    x <- fromList l
    r <- popBack x
    toList x

testPushBack :: [Int] -> [Int]
testPushBack l = runST $ do
    x <- fromList l
    pushBack x 1
    pushBack x 2
    pushBack x 3
    pushBack x 4
    pushBack x 5
    pushBack x 6
    pushBack x 7
    pushBack x 8
    pushBack x 9
    pushBack x 10
    toList x
