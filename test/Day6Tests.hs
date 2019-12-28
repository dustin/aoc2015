module Day6Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day6

testPart1 :: Assertion
testPart1 = assertEqual "" 400410 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 15343601 =<< part2

testPart1ST :: Assertion
testPart1ST = assertEqual "" 400410 =<< (part1' <$> getInput)

testPart2ST :: Assertion
testPart2ST = assertEqual "" 15343601 =<< (part2' <$> getInput)

tests :: [TestTree]
tests = [
  -- testCase "part1" testPart1,
  -- testCase "part2" testPart2,
  testCase "part1 ST" testPart1ST,
  testCase "part2 ST" testPart2ST
  ]
