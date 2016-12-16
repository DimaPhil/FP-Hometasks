module Main where

import           Criterion.Main
import qualified Data.DList     as DL (append, fromList, toList)
import           Data.Foldable  (toList)
import qualified Data.List      as DATAL (sort)
import qualified Data.Sequence  as SEQ (fromList, sort, (><))
import qualified DynamicArray   as DA (concat, fromList, sort, toList)

lst :: [Int]
lst = [1,2..1000]

lstr :: [Int]
lstr = [1000,999..1]

main :: IO ()
main = defaultMain
  [ bgroup "concat1000"
    [ bench "dynamic" $ nf (DA.concat lst) lst
    , bench "dlist" $ nf DL.toList (DL.append (DL.fromList lst) (DL.fromList lst))
    , bench "list" $ nf id (lst ++ lst)
    , bench "seq" $ nf toList (SEQ.fromList lst SEQ.>< SEQ.fromList lst )
    ]
  , bgroup "sort1000"
    [ bench "dynamic" $ nf DA.sort lstr
    --, bench "dlist" $ nf (DL.toList . DL.sort) (DL.fromList lstr)
    , bench "list" $ nf DATAL.sort lstr
    , bench "seq" $ nf (toList . SEQ.sort) (SEQ.fromList lstr)
    ]
  ]
