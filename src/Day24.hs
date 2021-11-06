module Day24 where

import           Advent.AoC                 (parseFile, snd3, thrd)
import           Data.List                  (sort)
import           Data.Monoid                (Product (..), Sum (..))
import           Text.Megaparsec            (many)
import           Text.Megaparsec.Char       (space)
import qualified Text.Megaparsec.Char.Lexer as L


getInput :: FilePath -> IO [Int]
getInput = parseFile (many (L.lexeme space L.decimal))

--               count,   weight,  product
type Packages = (Sum Int, Sum Int, Product Int)

pSum :: Packages -> Int
pSum = getSum . snd3

pProduct :: Packages -> Int
pProduct = getProduct . thrd

aPkg :: Int -> Packages
aPkg p = (1, Sum p, Product p)

search :: Int -> [Int] -> Int
search n ns = pProduct . head . sort . go mempty $ ns
  where
    goal = sum ns `div` n

    go :: Packages -> [Int] -> [Packages]
    go pkgs _ | pSum pkgs == goal = [pkgs]
    go _ [] = []
    go pkgs (x:_) | pSum pkgs + x > goal = []
    go pkgs (x:xs) = go (pkgs <> aPkg x) xs <> go pkgs xs

part1 :: IO Int
part1 = search 3 <$> getInput "input/day24"

part2 :: IO Int
part2 = search 4 <$> getInput "input/day24"
