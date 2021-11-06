module Day24Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day24

testPart1 :: Assertion
testPart1 = assertEqual "" 10439961859 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 72050269 =<< part2


tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
