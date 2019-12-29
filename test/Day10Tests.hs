module Day10Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day10

{-
1 becomes 11 (1 copy of digit 1).
11 becomes 21 (2 copies of digit 1).
21 becomes 1211 (one 2 followed by one 1).
1211 becomes 111221 (one 1, one 2, and two 1s).
111221 becomes 312211 (three 1s, two 2s, and one 1).

-}

testEx1 :: Assertion
testEx1 = assertEqual "" ["1", "11", "21", "1211", "111221", "312211"] (take 6 $ iterate expand "1")

testPart1 :: Assertion
testPart1 = assertEqual "" 329356 part1

testPart2 :: Assertion
testPart2 = assertEqual "" 4666278 part2


tests :: [TestTree]
tests = [
  testCase "ex 1" testEx1,
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
