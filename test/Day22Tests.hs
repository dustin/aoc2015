module Day22Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day22

testPart1 :: Assertion
testPart1 = assertEqual "" 1269 part1

testPart2 :: Assertion
testPart2 = assertEqual "" 1309 part2


tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
