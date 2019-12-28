{-# LANGUAGE LambdaCase #-}

module Day3 where

import           Data.List       (transpose)
import           Data.List.Extra (chunksOf)
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Advent.TwoD

getInput :: IO [Dir]
getInput = map (\case
                   '^' -> N
                   '>' -> E
                   'v' -> S
                   '<' -> W
                   x -> error ("invalid direction: " <> show x))
           <$> readFile "input/day3"

houses :: [Dir] -> Set Point
houses = Set.fromList . scanl (flip fwd) (0,0)

part1 :: IO Int
part1 = length . houses <$> getInput

part2 :: IO Int
part2 = length . Set.unions . fmap houses . transpose . chunksOf 2 <$> getInput
