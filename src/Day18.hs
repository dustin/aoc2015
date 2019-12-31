{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module Day18 where

import           Advent.AoC         (parseGrid)
import           Advent.TwoD
import qualified Data.Array.Unboxed as A
import qualified Data.Map.Strict    as Map
import           Data.Semigroup     (Sum (..))

type World = A.Array Point Int

maxX, maxY :: Int
maxX = 99
maxY = 99

mkWorld :: [(Point,Int)] -> World
mkWorld = A.array ((0,0),(99,99))

getInput :: IO World
getInput = mkWorld . Map.toList . parseGrid (\x -> if x == '#' then 1 else 0) <$> readFile "input/day18"

neighbors :: Point -> [Point]
neighbors p = [(x,y) | (x,y) <- aroundD p, x >= 0 && y >= 0 && x <= maxX && y <= maxY]

automata :: World -> World
automata w = mkWorld . map f $ A.assocs w
  where f (p, 1)
          | nc p `elem` [2,3] = (p, 1)
          | otherwise = (p, 0)
        f (p, _)
          | nc p == 3 = (p, 1)
          | otherwise = (p, 0)
        nc :: Point -> Int
        nc p = getSum . foldMap (Sum . (w A.!)) $ neighbors p

automata2 :: World -> World
automata2 w = mkWorld $ (map f $ A.assocs w) <> [((x,y),1) | x <- [0,maxX], y <- [0,maxY]]
  where f (p, 1)
          | nc p `elem` [2,3] = (p, 1)
          | otherwise = (p, 0)
        f (p, _)
          | nc p == 3 = (p, 1)
          | otherwise = (p, 0)
        nc :: Point -> Int
        nc p = getSum . foldMap (Sum . (w A.!)) $ neighbors p

part1 :: IO Int
part1 = sum . (!! 100) . iterate automata <$> getInput

part2 :: IO Int
part2 = sum . (!! 100) . iterate automata2 . munge <$> getInput
  where
    munge w = mkWorld (A.assocs w <> [((x,y),1) | x <- [0,maxX], y <- [0,maxY]])
