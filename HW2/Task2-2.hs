module Test where

data Equipment = Equipment Int Int Int deriving Show

upgradeHealth :: Equipment -> Int
upgradeHealth (Equipment uh _ _) = uh

upgradeAttack :: Equipment -> Int
upgradeAttack (Equipment _ ua _) = ua

upgradeDefence :: Equipment -> Int
upgradeDefence (Equipment _ _ ud) = ud

data Player = Player Int Int Int deriving Show
data Monster = Monster Int Int Int Equipment deriving Show

gloriousBattle :: Player -> [Monster] -> Player
gloriousBattle p [] = p
gloriousBattle p@(Player phealth pattack pdefence) ((Monster mhealth mattack mdefence mequipment):ms)
  | phealth <= 0                             = p
  | pattack >= mhealth - mdefence            = gloriousBattle (Player newHealth newAttack newDefence) ms
  | pattack < mdefence && mattack < pdefence = error "The battle is infinite"
  | otherwise                                = gloriousBattle afterRoundPlayer (afterRoundMonster:ms) where
      newHealth = phealth + upgradeHealth mequipment
      newAttack = pattack + upgradeAttack mequipment
      newDefence = pdefence + upgradeDefence mequipment
      afterRoundPlayerHealth = phealth - (max 0 $ mattack - pdefence)
      afterRoundPlayer = Player afterRoundPlayerHealth pattack pdefence
      afterRoundMonsterHealth = mhealth - (max 0 $ pattack - mdefence)
      afterRoundMonster = Monster afterRoundMonsterHealth mattack mdefence mequipment
      