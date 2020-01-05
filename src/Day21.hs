module Day21 where

import           Control.Lens
import           Control.Monad (guard)
import qualified Data.Set      as Set

data Player = Player {
  hitPoints :: Int,
  armor     :: Int,
  damage    :: Int
  } deriving Show

-- Name, Cost, Damage, Armor
type Item = (String, Int, Int, Int)

weaponsForSale, armorForSale, ringsForSale :: [Item]

weaponsForSale = [
  ("Dagger",        8,     4,       0),
  ("Shortsword",   10,     5,       0),
  ("Warhammer",    25,     6,       0),
  ("Longsword",    40,     7,       0),
  ("Greataxe",     74,     8,       0)]

armorForSale = [
  ("Nothing",       0,     0,       0),
  ("Leather",      13,     0,       1),
  ("Chainmail",    31,     0,       2),
  ("Splintmail",   53,     0,       3),
  ("Bandedmail",   75,     0,       4),
  ("Platemail",   102,     0,       5)]

ringsForSale = [
  ("Nothing L",     0,     0,       0),
  ("Nothing R",     0,     0,       0),
  ("Damage +1",    25,     1,       0),
  ("Damage +2",    50,     2,       0),
  ("Damage +3",   100,     3,       0),
  ("Defense +1",   20,     0,       1),
  ("Defense +2",   40,     0,       2),
  ("Defense +3",   80,     0,       3)]

-- o attacks d.  Returns new d.
attack :: Player -> Player -> Player
attack Player{damage} d@Player{hitPoints, armor} =
  d{hitPoints=max 0 (hitPoints - (max 1 (damage - armor)))}

-- Battle between two players.  True if the first player wins.
battle :: Player -> Player -> Bool
battle = go True
  where
    go r a b
      | isDead = r
      | otherwise = go (not r) b' a
      where b' = attack a b
            isDead = hitPoints b' == 0

applyItem :: Item -> Player -> Player
applyItem (_, _, dam, arm) p@Player{..} = p{armor=armor + arm, damage=damage + dam}

purchases :: [[Item]]
purchases = Set.toList . Set.fromList $ do
  w <- weaponsForSale
  a <- armorForSale
  r1 <- ringsForSale
  r2 <- ringsForSale
  guard $ r1 /= r2
  pure $ filter (\(_, cost, _, _) -> cost > 0) [w, a, r1, r2]

boss :: Player
boss = Player 109 2 8

withItems :: [Item] -> (Int, Player)
withItems its = (sumOf (folded . _2) its, foldr applyItem (Player 100 0 0) its)

wins :: Player -> Bool
wins = flip battle boss

part1 :: Int
part1 = minimum . fmap fst . filter (wins . snd) $ map withItems purchases

part2 :: Int
part2 = maximum . fmap fst . filter (not . wins . snd) $ map withItems purchases

