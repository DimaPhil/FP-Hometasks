module Test where

data Equipment = Equipment { upgradeHealth :: Int, upgradeAttack :: Int, upgradeDefence :: Int }
  deriving Show
data Creature = Player { health :: Int, attack :: Int, defence :: Int }
              | Monster { health :: Int, attack :: Int, defence :: Int, equipment :: Equipment }
  deriving Show

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

gloriousBattle :: Creature -> [Creature] -> Creature
gloriousBattle Monster{} _ = error "First parameter should be Player"
gloriousBattle _ (Player{}:_) = error "Second parameter should be the list of Monsters, not Players"
gloriousBattle p [] = p
gloriousBattle p (m:ms)
  | (p & health) <= 0                            = p
  | (p & attack) >= (m & health) - (m & defence) = gloriousBattle (Player newHealth newAttack newDefence) ms
  | (p & attack) < (m & defence) && (m & attack) < (p & defence) = error "The battle is infinite"
  | otherwise                                    = gloriousBattle afterRoundPlayer (afterRoundMonster:ms) where
      newHealth = (p & health) + (m & equipment & upgradeHealth)
      newAttack = (p & attack) + (m & equipment & upgradeAttack)
      newDefence = (p & defence) + (m & equipment & upgradeDefence)
      afterRoundPlayerHealth = (p & health) - (max 0 $ (m & attack) - (p & defence))
      afterRoundPlayer = p { health = afterRoundPlayerHealth }
      afterRoundMonsterHealth = (m & health) - (max 0 $ (p & attack) - (m & defence))
      afterRoundMonster = m { health = afterRoundMonsterHealth }
      