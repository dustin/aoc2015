module Day9Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day9

testEx1 :: Assertion
testEx1 = assertEqual "" (Just 605) =<< (fmap fst . findPath <$> getInput "input/day9.example")

testPart1 :: Assertion
testPart1 = assertEqual "" (Just 207) =<< part1

testEx2 :: Assertion
testEx2 = assertEqual "" 982 =<< (part2' <$> getInput "input/day9.example")

testPart2 :: Assertion
testPart2 = assertEqual "" 804 =<< part2


tests :: [TestTree]
tests = [
  testCase "ex 1" testEx1,
  testCase "part1" testPart1,
  testCase "ex 2" testEx2,
  testCase "part2" testPart2
  ]
