module Day12 where

import           Control.Lens
import           Control.Lens.Plated (deep)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Maybe          (fromJust)
import           Data.Scientific     (Scientific)

getInput :: IO Value
getInput = fromJust <$> decodeFileStrict "input/day12"

-- Sum of all the numbers in a JSON object.
part1 :: IO Scientific
part1 = sumOf (deep _Number) <$> getInput

-- My first, super verbose version.
-- part1 = getInput >>= \j -> pure . sum $ (j ^.. biplate :: [Scientific])

-- Sum of all the numbers in a JSON object after throwing away any
-- objects that have a *value* of "red"
part2 :: IO Scientific
part2 = sumOf (deep _Number) . transform killRed <$> getInput
  where
    killRed o | elemOf (members . _String) "red" o = Null
    killRed x = x
