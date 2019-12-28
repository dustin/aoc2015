module Day5 where

import           Data.List (group, isInfixOf, sort)

getInput :: IO [String]
getInput = lines <$> readFile "input/day5"

part1 :: IO Int
part1 = length . filter isNice <$> getInput
  where
    isNice w = length (vowels w) > 2 && hasDouble w && hasNo w ["ab", "cd", "pq", "xy"]
    vowels = filter (`elem` ("aeiou"::String))
    hasDouble = not . null . filter ((> 1) . length) . group
    hasNo w = all (not . (`isInfixOf` w))

part2 :: IO Int
part2 = length . filter isNice <$> getInput
  where
    isNice w = hasPair (nonOverlapping w) && hasSandwich w
    hasSandwich (a:b:c:xs)
      | a == c = True
      | otherwise = hasSandwich (b:c:xs)
    hasSandwich _ = False

    nonOverlapping (a:b:c:xs)
      | a == b && b == c = nonOverlapping xs
      | otherwise = a : nonOverlapping (b:c:xs)
    nonOverlapping xs = xs

    hasPair w = not . null . filter ((> 1) . length) . group . sort . zip w $ tail w
