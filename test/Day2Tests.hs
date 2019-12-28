module Day2Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day2

testPart1 :: Assertion
testPart1 = assertEqual "" 1606483 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 3842356 =<< part2

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
