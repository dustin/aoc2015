module Day11Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day11

testPart1 :: Assertion
testPart1 = assertEqual "" "vzbxxyzz" part1

testPart2 :: Assertion
testPart2 = assertEqual "" "vzcaabcc" part2


tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
