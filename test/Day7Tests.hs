module Day7Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Word             (Word16)

import           Day7

testPart1 :: Assertion
testPart1 = assertEqual "" 16076 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 2797 =<< part2

oneWant :: Map String Word16
oneWant = Map.fromList [
  ("d", 72),
  ("e", 507),
  ("f", 492),
  ("g", 114),
  ("h", 65412),
  ("i", 65079),
  ("x", 123),
  ("y", 456)]

test1Ex :: Assertion
test1Ex = do
  inst <- getInput "input/day7.sample"
  assertEqual "" oneWant (eval inst)

tests :: [TestTree]
tests = [
  testCase "1 ex" test1Ex,
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
