module Day17 where

import           Advent.Search
import           Data.List     (sort, tails)
import           Data.Maybe    (mapMaybe)
import qualified Data.Set      as Set

getInput :: IO [Int]
getInput = map read . words <$> readFile "input/day17"

ex :: [Int]
ex = [20, 15, 10, 5, 5]

data BState = BState {
  used      :: [(Char,Int)],
  remaining :: [(Char,Int)]
  } deriving (Show, Eq, Ord)

fit :: Int -> [Int] -> [[Int]]
fit goal nums = (fmap.fmap) snd . Set.toList . Set.fromList . mapMaybe matches $ bfs nf (BState [] (zip ['a'..] $ reverse . sort $ nums))
  where
    matches BState{used}
      | sum ns == goal = Just used
      | otherwise = Nothing
      where ns = snd <$> used
    nf BState{..}
      | null next = []
      | otherwise = (BState (x:used) <$> tn) <> (BState used <$> tn)
      where s = sum (snd <$> used)
            next = dropWhile ((> (goal - s)) . snd) remaining
            tn = tail . tails $ next
            (x:_) = next

part1 :: [Int] -> Int
part1 = length . fit 150

part2 :: [Int] -> Int
part2 = length . filter ((== 4) . length) . fit 150
