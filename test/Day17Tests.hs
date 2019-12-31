module Day17Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day17

testPart1 :: Assertion
testPart1 = assertEqual "" 4372 =<< (part1 <$> getInput)

testPart2 :: Assertion
testPart2 = assertEqual "" 4 =<< (part2 <$> getInput)


tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
