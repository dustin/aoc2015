{-# LANGUAGE TemplateHaskell #-}

module Day22 where

import           Control.Applicative ((<|>))
import           Control.Lens
import           Control.Monad.State
import           Data.Either         (isRight)
import           Data.Foldable       (fold)
import           Data.Function       (on)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Tuple          (swap)

{-
Magic Missile costs 53 mana. It instantly does 4 damage.

Drain costs 73 mana. It instantly does 2 damage and heals you for 2
hit points.

Shield costs 113 mana. It starts an effect that lasts for 6
turns. While it is active, your armor is increased by 7.

Poison costs 173 mana. It starts an effect that lasts for 6 turns. At
the start of each turn while it is active, it deals the boss 3 damage.

Recharge costs 229 mana. It starts an effect that lasts for 5
turns. At the start of each turn while it is active, it gives you 101
new mana.
-}

data Magic = Missile | Drain | Shield | Poison | Recharge deriving (Enum, Eq, Ord, Bounded, Show)

type Modification = Player -> Player

data Player = Player {
  _hitPoints   :: !Int
  , _armor     :: !Int
  , _offense   :: !(Either Int Int) -- damage, mana
  , _effects   :: ![[Modification]]
  , _active    :: !(Set Magic) -- active spells
  , _manaSpent :: !Int
  }

makeLenses ''Player

cost :: Magic -> Int
cost Missile  = 53
cost Drain    = 73
cost Shield   = 113
cost Poison   = 173
cost Recharge = 229
{-# INLINE cost #-}

spendMana :: Magic -> Player -> Player
spendMana m p = p & offense . _Right -~ c & manaSpent +~ c
  where c = cost m

infill :: [a] -> [[a]] -> [[a]]
infill new old = zipWith (:) new (old <> repeat [])

deleteAfter :: Int -> Magic -> [Modification]
deleteAfter x m = replicate x id <> [over active (m `Set.delete`)]

applyMagic :: Magic -> Player -> Player -> (Player, Player)
-- missle costs 53 and does 4 damage
applyMagic m@Missile p1@Player{_offense} p2@Player{_hitPoints} =
  (spendMana m p1, p2 & hitPoints -~ 4)
-- drain costs 73 and does 2 damage then heals 2
applyMagic m@Drain p1 p2 =
  (spendMana m p1 & hitPoints +~ 2, p2 & hitPoints -~ 2)
-- shield costs 113 and lasts 6 turns over which armor is increased by 7
applyMagic m@Shield p1 p2 =
  (spendMana m p1 & armor +~ 7
                  & effects %~ infill (replicate 6 id <> [armor -~ 7])
                  & effects %~ infill (deleteAfter 6 m)
                  & active <>~ Set.singleton m,
   p2)
-- poison costs 173 and lasts 6 turns, giving the enemy 3 damage per cycle
applyMagic m@Poison p1@Player{_offense} p2@Player{_effects} =
  (spendMana m p1 & active <>~ Set.singleton m
                  & effects %~ infill (deleteAfter 6 m),
   p2 & hitPoints -~ 3 &  effects %~ (infill (replicate 6 (over hitPoints (subtract 3)))))
-- recharge costs 229 and gives 101 over the next 5 turns
applyMagic m@Recharge p1@Player{_offense, _effects} p2 =
  (spendMana m p1 & effects %~ infill (replicate 5 (over (offense . _Right) (+ 101)))
                  & effects %~ infill (deleteAfter 5 m)
                  & active <>~ Set.singleton m,
   p2)

availableSpells :: Player -> [Magic]
availableSpells Player{_offense=Right mana, _active} = [x | x <- [minBound..], cost x <= mana,
                                                        x `Set.notMember` _active ]
availableSpells _ = []

isPlayer :: Player -> Bool
isPlayer = isRight . _offense

runTimers :: Player -> Player
runTimers p@Player{_effects=[]}     = p
runTimers p@Player{_effects=(x:xs)} = foldr ($) p x & effects .~ xs

{-
Hit Points: 58
Damage: 9
-}

boss :: Player
boss = Player 58 0 (Left 9) [] mempty 0

data FinalState = FinalState {
  _winner  :: !Player
  , _loser :: !Player
  }

makeLenses ''FinalState

attack :: Player -> Player -> [(Player, Player)]
attack p1@Player{_offense = Left damage} p2@Player{_hitPoints, _armor} =
  [(p2{_hitPoints=max 0 (_hitPoints - (max 1 (damage - _armor)))}, p1)]
attack p1 p2 = [ swap (applyMagic m p1 p2) | m <- availableSpells p1 ]

allGames :: Player -> Player -> [FinalState]
allGames p1 p2 = evalState (go p1 p2) maxBound
  where
    go :: Player -> Player -> State Int [FinalState]
    go a b
      | bossDead = do
          mn <- get
          if currentSpent < mn
            then put currentSpent *> pure [FinalState a b]
            else pure []
      | otherDead = pure []
      | i'mOut = pure []
      | otherwise = do
          mn <- get
          if currentSpent >= mn
            then pure []
            else fold <$> (traverse (uncurry go) $ attack a' b')
      where
        bossDead = _hitPoints b <= 0
        otherDead = _hitPoints a <= 0
        currentSpent = (max `on` _manaSpent) a b
        i'mOut = ((a ^? offense . _Right) <|> (b ^? offense . _Right)) == Just 0
        a' = runTimers a
        b' = runTimers b

part1 :: Int
part1 = minimum . fmap (_manaSpent . _winner) $ filter (isPlayer . _winner) $ allGames player boss
  where
    player = Player 50 0 (Right 500) [] mempty 0

part2 :: Int
part2 = minimum . fmap (_manaSpent . _winner) $ filter (isPlayer . _winner) $ allGames player boss
  where
    player = Player 50 0 (Right 500) (cycle [[hitPoints -~ 1], [id]]) mempty 0
