module Day8Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day8

testPart1 :: Assertion
testPart1 = assertEqual "" 1371 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 2117 =<< part2

testEx1 :: Assertion
testEx1 = assertEqual "" 12 =<< (part1' <$> getInput "input/day8.example")

testEx2 :: Assertion
testEx2 = assertEqual "" 19 =<< (part2' <$> getInput "input/day8.example")


tests :: [TestTree]
tests = [
  testCase "ex 1" testEx1,
  testCase "part1" testPart1,
  testCase "ex 2" testEx2,
  testCase "part2" testPart2
  ]
