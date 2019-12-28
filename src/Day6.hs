{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module Day6 where

import           Advent.AoC                 (parseFile)
import           Advent.TwoD
import           Control.Applicative        (liftA2, (<|>))
import           Control.Monad.ST
import qualified Data.Array.IArray          as IA
import qualified Data.Array.MArray          as MA
import qualified Data.Array.ST              as MA
import           Data.List                  (foldl')
import qualified Data.Map.Strict            as Map
import           Text.Megaparsec            (endBy, try)
import           Text.Megaparsec.Char.Lexer (decimal)

data Range = Range Point Point deriving Show

data Command = TurnOn Range | TurnOff Range | Toggle Range deriving Show

getInput :: IO [Command]
getInput = parseFile parseAll "input/day6"
  where parseAll = parseCommand `endBy` "\n"
        parseCommand = cons <*> parseRange
        cons = TurnOn <$ try "turn on "
               <|> TurnOff <$ try "turn off "
               <|> Toggle <$ "toggle "
        parseRange = Range <$> parsePoint <*> (" through " *> parsePoint)
        parsePoint = liftA2 (,) decimal ("," *> decimal)

expand :: Range -> [Point]
expand (Range (x1,y1) (x2,y2)) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

part1' :: [Command] -> Int
part1' cmds = length . filter id . IA.elems $ MA.runSTUArray go
  where go :: ST s (MA.STUArray s Point Bool)
        go = do
          a <- MA.newArray ((0,0), (999,999)) False
          mapM_ (eval a) cmds
          pure a
        eval :: MA.STUArray s Point Bool -> Command -> ST s ()
        eval a (TurnOn rs)  = mapM_ (write1 True a) (expand rs)
        eval a (TurnOff rs) = mapM_ (write1 False a) (expand rs)
        eval a (Toggle rs)  = mapM_ (toggle a) (expand rs)

        write1 v a p = MA.writeArray a p v
        toggle a p = MA.writeArray a p . not =<< MA.readArray a p


part2' :: [Command] -> Int
part2' cmds = sum . IA.elems $ MA.runSTUArray go
  where go :: ST s (MA.STUArray s Point Int)
        go = do
          a <- MA.newArray ((0,0), (999,999)) 0
          mapM_ (`eval` a) cmds
          pure a
        eval (TurnOn rs)  = evaln succ rs
        eval (TurnOff rs) = evaln (max 0 . pred) rs
        eval (Toggle rs)  = evaln (+ 2) rs

        {-# INLINE evaln #-}
        evaln f rs a = mapM_ (eval1 f a) (expand rs)

        eval1 f a p = MA.writeArray a p . f =<< MA.readArray a p

part1 :: IO Int
part1 = length . Map.filter id . foldl' (flip eval) im <$> getInput
  where
    im = Map.fromList [((x,y),False) | x <- [0..999], y <- [0..999]]
    eval (TurnOn rs) = Map.union (Map.fromList $ fmap (,True) (expand rs))
    eval (TurnOff rs) = Map.union (Map.fromList $ fmap (,False) (expand rs))
    eval (Toggle rs) = Map.unionWith (\_ x -> not x) (Map.fromList $ fmap (,True) (expand rs))

part2 :: IO Int
part2 = sum . Map.elems . foldl' (flip eval) im <$> getInput
  where
    im = Map.fromList [((x,y),0) | x <- [0..999], y <- [0..999]]
    eval (TurnOn rs) = Map.unionWith (+) (Map.fromList $ fmap (,1) (expand rs))
    eval (TurnOff rs) = Map.unionWith (\a b -> max 0 (a + b)) (Map.fromList $ fmap (,-1) (expand rs))
    eval (Toggle rs) = Map.unionWith (+) (Map.fromList $ fmap (,2) (expand rs))
