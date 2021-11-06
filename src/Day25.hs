module Day25 where

at :: (Int, Int) -> Int
at (1,1) = 20151125
at p = at (prevLoc p) * 252533 `mod` 33554393
  where
    prevLoc (1,x) = (x - 1, 1)
    prevLoc (x,y) = (x - 1, y + 1)

part1 :: Int
part1 = at (3019, 3010)
