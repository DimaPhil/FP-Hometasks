{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coins where

import           Data.Monoid

newtype Coin color = Coin { getCoin :: Integer } deriving (Show, Num)

data Blue
data Red

class Color a where
  color :: a -> Integer

instance Color Blue where
  color _ = 0

instance Color Red where
  color _ = 1

blue :: Blue
blue = undefined :: Blue
red :: Red
red = undefined :: Red

getColor :: Coin color -> color
getColor _ = undefined

createCoins :: color -> Integer -> Coin color
createCoins _ = Coin

c1 :: Coin Blue
c1 = createCoins blue 10
c2 :: Coin Blue
c2 = createCoins blue 9
c3 :: Coin Red
c3 = Coin 5 :: Coin Red

instance Monoid (Coin a) where
  mempty      = Coin 0
  mappend (Coin x) (Coin y) = Coin (x + y)

cmp :: (Color a, Color b) => Coin a -> Coin b -> Ordering
cmp coin1@(Coin x) coin2@(Coin y) = case compare (color $ getColor coin1) (color $ getColor coin2) of
  EQ     -> compare x y
  value  -> value
