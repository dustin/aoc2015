module Day11 where

import           Data.Foldable (toList)
import           Data.List     (group)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Password = Seq Char

incr :: Password -> Password
incr pw = go pw (length pw -1)
  where go p i
          | p `Seq.index` i == 'z' = go (Seq.update i 'a' p) (i - 1)
          | otherwise = Seq.adjust inc i p
        inc 'h' = 'j'
        inc 'k' = 'm'
        inc 'n' = 'p'
        inc c   = succ c

valid :: Password -> Bool
valid pw = hasInc pwl && hasPairs pwl
  where pwl = toList pw
        hasInc (a:b:c:xs)
          | succ a == b && succ b == c = True
          | otherwise = hasInc (b:c:xs)
        hasInc _ = False
        hasPairs = (> 1) . length . filter ((> 1) . length) . group

input :: Password
input = "vzbxkghb"

part1 :: String
part1 = toList . head . filter valid . iterate incr $ input

part2 :: String
part2 = toList . head . drop 1 . filter valid . iterate incr $ input
