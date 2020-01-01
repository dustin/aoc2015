module Day19Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day19

testPart1 :: Assertion
testPart1 = assertEqual "" 576 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 207 =<< part2


tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
