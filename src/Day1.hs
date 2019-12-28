{-# LANGUAGE LambdaCase #-}

module Day1 where

import           Data.List (findIndex)

getInput :: IO [Int]
getInput = map (\case
                   '(' -> 1
                   ')' -> -1
                   _ -> 0) <$> readFile "input/day1"

part1 :: IO Int
part1 = sum <$> getInput

part2 :: IO (Maybe Int)
part2 = findIndex (== -1) . scanl (+) 0 <$> getInput
