module Test where

data Equipment = Equipment
  { upgradeHealth :: Int
  , upgradeAttack :: Int
  , upgradeDefence :: Int 
  } deriving Show

data Player = Player 
  { phealth :: Int
  , pattack :: Int
  , pdefence :: Int
  } deriving Show

data Monster = Monster 
  { mhealth :: Int
  , mattack :: Int
  , mdefence :: Int
  , mequipment :: Equipment 
  } deriving Show

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

gloriousBattle :: Player -> [Monster] -> Player
gloriousBattle p [] = p
gloriousBattle p (m:ms)
  | (p & phealth) <= 0                              = p
  | (p & pattack) >= (m & mhealth) - (m & mdefence) = gloriousBattle newPlayer ms
  | (p & pattack) < (m & mdefence) && (m & mattack) < (p & pdefence) = error "The battle is infinite"
  | otherwise = gloriousBattle afterRoundPlayer (afterRoundMonster:ms) where
      newHealth = (p & phealth) + (m & mequipment & upgradeHealth)
      newAttack = (p & pattack) + (m & mequipment & upgradeAttack)
      newDefence = (p & pdefence) + (m & mequipment & upgradeDefence)
      newPlayer = Player newHealth newAttack newDefence
      afterRoundPlayerHealth = (p & phealth) - (max 0 $ (m & mattack) - (p & pdefence))
      afterRoundPlayer = p { phealth = afterRoundPlayerHealth }
      afterRoundMonsterHealth = (m & mhealth) - (max 0 $ (p & pattack) - (m & mdefence))
      afterRoundMonster = m { mhealth = afterRoundMonsterHealth }
      