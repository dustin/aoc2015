module Day2 where

import           Advent.AoC                 (parseFile)
import           Control.Applicative        (liftA3)
import           Text.Megaparsec            (endBy)
import           Text.Megaparsec.Char.Lexer (decimal)

type Box = (Int,Int,Int)

getInput :: IO [Box]
getInput = parseFile parseAll "input/day2"
  where parseAll = parseBox `endBy` "\n"
        parseBox = liftA3 (,,) decimal ("x" *> decimal) ("x" *> decimal)

ordered :: Box -> Box
ordered (l,w,h) = (min (min l w) h,
                   max (max (min l w) (min w h)) (min l h),
                   max (max l w) h)

wreq :: Box -> Int
wreq box@(l,w,h) = 2*l*w + 2*w*h + 2*h*l + small
  where small = let (a,b,_) = ordered box in a * b

part1 :: IO Int
part1 = sum . map wreq <$> getInput

breq :: Box -> Int
breq bin = 2*l + 2*w + l*w*h
  where (l,w,h) = ordered bin

part2 :: IO Int
part2 = sum . map breq <$> getInput
