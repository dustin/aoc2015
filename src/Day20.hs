module Day20 where

import           Data.List (union)


divisors :: Int -> [Int]
divisors 1 = [1]
divisors 2 = [1, 2]
divisors n = let lower = [x | x <- [2..isqrt n], n `mod` x == 0] in
               1 : n : union lower (map (div n) lower)
  where isqrt = ceiling . sqrt . fromIntegral

elfDelivery :: Int -> Int
elfDelivery = sum . map (10 *) . divisors

part1 :: Int
part1 = fst . head . filter ((>= 33100000) . snd) . map (\x -> (x, elfDelivery x)) $ [1..]

elfDelivery2 :: Int -> Int
elfDelivery2 n = let divs = divisors n
                     divs' = filter ((>= n) . (* 50)) divs in
                   sum . map (* 11) $ divs'

part2 :: Int
part2 = fst . head . filter ((>= 33100000) . snd) . map (\x -> (x, elfDelivery2 x)) $ [1..]
