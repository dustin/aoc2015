import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import qualified Day10Tests
import qualified Day11Tests
import qualified Day12Tests
import qualified Day13Tests
import qualified Day14Tests
import qualified Day15Tests
import qualified Day1Tests
import qualified Day2Tests
import qualified Day3Tests
import qualified Day4Tests
import qualified Day5Tests
import qualified Day6Tests
import qualified Day7Tests
import qualified Day8Tests
import qualified Day9Tests

tests :: [TestTree]
tests = [
  testGroup "day1" Day1Tests.tests,
  testGroup "day2" Day2Tests.tests,
  testGroup "day3" Day3Tests.tests,
  testGroup "day4" Day4Tests.tests,
  testGroup "day5" Day5Tests.tests,
  testGroup "day6" Day6Tests.tests,
  testGroup "day7" Day7Tests.tests,
  testGroup "day8" Day8Tests.tests,
  testGroup "day9" Day9Tests.tests,
  testGroup "day10" Day10Tests.tests,
  testGroup "day11" Day11Tests.tests,
  testGroup "day12" Day12Tests.tests,
  testGroup "day13" Day13Tests.tests,
  testGroup "day14" Day14Tests.tests,
  testGroup "day15" Day15Tests.tests
    ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
