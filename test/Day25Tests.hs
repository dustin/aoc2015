module Day25Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day25

testPart1 :: Assertion
testPart1 = assertEqual "" 8997277 part1

tests :: [TestTree]
tests = [
  testCase "part1" testPart1
  ]
