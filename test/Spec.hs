import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import qualified Day1Tests

tests :: [TestTree]
tests = [
  testGroup "day1" Day1Tests.tests
    ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
