module Day8 where

import           Control.Lens
import           Data.Char    (chr, digitToInt)

getInput :: FilePath -> IO [(String, String)]
getInput fp = map (\x -> (x, dec ((tail . init) x))) . lines <$> readFile fp
  where
    dec ('\\':'"':xs)     = '"':dec xs
    dec ('\\':'\\':xs)    = '\\':dec xs
    dec ('\\':'x':a:b:xs) = unhex a b : dec xs
    dec (x:xs)            = x : dec xs
    dec []                = []

    unhex a b = chr (digitToInt a * 16 + digitToInt b)

part1' :: [(String, String)] -> Int
part1' inp = let (as, bs) = unzip $ fmap (over both length) inp
             in sum as - sum bs


part1 :: IO Int
part1 = part1' <$> getInput "input/day8"

part2' :: [(String, String)] -> Int
part2' inp = lencd - lraw
  where
    raw = fst <$> inp
    lraw = sum . fmap length $ raw
    encd = ('"':) . enc <$> raw
    lencd = sum . fmap length $ encd

    enc [] = "\""
    enc (x:xs)
      | x == '"'  = "\\\"" <> enc xs
      | x == '\\' = "\\\\" <> enc xs
      | otherwise = [x]  <> enc xs

part2 :: IO Int
part2 = part2' <$> getInput "input/day8"
