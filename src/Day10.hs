module Day10 where

import           Data.Char (intToDigit)
import           Data.List (group)

expand :: String -> String
expand = e . group
  where
    e = foldMap (\l@(c:_) -> intToDigit (length l) : c : [])

part1 :: Int
part1 = length $ iterate expand "3113322113" !! 40

part2 :: Int
part2 = length $ iterate expand "3113322113" !! 50
