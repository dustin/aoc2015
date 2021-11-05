{-# LANGUAGE TemplateHaskell #-}

module Day22 where

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable       (traverse_)
import           Data.List           (partition)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

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

data Player = Player {
  _pHitPoints :: !Int
  , _mana     :: !Int
  , _spent    :: !Int
  , _inUse    :: Map Magic Int
  } deriving Show

class HasHP t where
  hp :: Lens' t Int

instance HasHP Player where
  hp = lens _pHitPoints (\p x -> p{_pHitPoints=x})

data Boss = Boss {
  _bHitPoints :: !Int,
  _attack     :: !Int
  } deriving Show

instance HasHP Boss where
  hp = lens _bHitPoints (\b x -> b{_bHitPoints=x})

makeLenses ''Player
makeLenses ''Boss

cost :: Magic -> Int
cost Missile  = 53
cost Drain    = 73
cost Shield   = 113
cost Poison   = 173
cost Recharge = 229
{-# INLINE cost #-}

spendMana :: Magic -> Player -> Player
spendMana m p = p & mana -~ c & spent +~ c
  where c = cost m

availableSpells :: Player -> [Magic]
availableSpells Player{_mana, _inUse} = [x | x <- [minBound..], cost x <= _mana,
                                         x `Map.notMember` _inUse ]

boss :: Boss
boss = Boss 58 9

player :: Player
player = Player 50 500 0 mempty

data GameState = GameState {
  _bestScore :: Int,
  _preFight  :: Int
  } deriving Show

makeLenses ''GameState

cast :: Magic -> Player -> Boss -> (Player, Boss)
cast m@Missile p b  = (spendMana m p, b & hp -~ 4)
cast m@Drain p b    = (spendMana m p & hp +~ 2, b & hp -~ 2)
cast m@Shield p b   = (spendMana m p & inUse . at m ?~ 6, b)
cast m@Poison p b   = (spendMana m p & inUse . at m ?~ 6, b)
cast m@Recharge p b = (spendMana m p & inUse . at m ?~ 5, b)

stillAlive :: HasHP t => t -> Bool
stillAlive p = p ^?! hp > 0

applyActive :: Player -> Boss -> (Player, Boss)
applyActive ip ib = _1 . inUse %~ sub $ Map.foldrWithKey apply (ip,ib) (pred <$> _inUse ip)
  where
    sub = Map.filter (> 0) . fmap pred
    apply Poison _ (p,b)   = (p, b & hp -~ 3)
    apply Recharge _ (p,b) = (p & mana +~ 101, b)
    apply _ _ x            = x

armor :: Player -> Int
armor Player{_inUse}
  | Map.member Shield _inUse = 7
  | otherwise = 0

aRound :: Player -> Boss -> State GameState [(Player, Boss)]
aRound ip b = do
  mn <- gets _bestScore
  pf <- gets _preFight
  let p = ip & hp -~ pf
      (ongoing, won) = partition (stillAlive . snd) $ do
        guard (stillAlive p)
        guard (_spent p < mn)
        let (p', b') = applyActive p b
        spell <- availableSpells p'
        let (p'', b'') = cast spell p' b'
        if stillAlive b'' then do
          let p''' = p'' & hp -~ max 1 (b'' ^?! attack - armor p'')
              (fp, fb) = applyActive p''' b''

          guard (stillAlive fp)
          pure (fp, fb)
          else pure (p'', b'')
  mapM_ (\(Player{_spent}, _) -> when (_spent < mn) $ bestScore #= _spent) won
  pure ongoing

allGames :: Int -> Player -> Boss -> GameState
allGames o ip ib = execState (go ip ib) (GameState maxBound o)
  where go p b = traverse_ (uncurry go) =<< aRound p b

part1 :: Int
part1 = _bestScore $ allGames 0 player boss

part2 :: Int
part2 = _bestScore $ allGames 1 player boss
