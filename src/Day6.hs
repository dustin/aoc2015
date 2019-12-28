module Day6 where

import           Advent.AoC                 (parseFile)
import           Advent.TwoD
import           Control.Applicative        (liftA2, (<|>))
import           Data.List                  (foldl')
import qualified Data.Map.Strict            as Map
import           Text.Megaparsec            (endBy, try)
import           Text.Megaparsec.Char.Lexer (decimal)

data Range = Range Point Point deriving Show

data Command = TurnOn Range | TurnOff Range | Toggle Range deriving Show

getInput :: IO [Command]
getInput = parseFile parseAll "input/day6"
  where parseAll = parseCommand `endBy` "\n"
        parseCommand = cons <*> parseRange
        cons = TurnOn <$ try "turn on "
               <|> TurnOff <$ try "turn off "
               <|> Toggle <$ "toggle "
        parseRange = Range <$> parsePoint <*> (" through " *> parsePoint)
        parsePoint = liftA2 (,) decimal ("," *> decimal)

expand :: Range -> [Point]
expand (Range (x1,y1) (x2,y2)) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

part1 :: IO Int
part1 = length . Map.filter id . foldl' (flip eval) im <$> getInput
  where
    im = Map.fromList [((x,y),False) | x <- [0..999], y <- [0..999]]
    eval (TurnOn rs) = Map.union (Map.fromList $ fmap (,True) (expand rs))
    eval (TurnOff rs) = Map.union (Map.fromList $ fmap (,False) (expand rs))
    eval (Toggle rs) = Map.unionWith (\_ x -> not x) (Map.fromList $ fmap (,True) (expand rs))

part2 :: IO Int
part2 = sum . Map.elems . foldl' (flip eval) im <$> getInput
  where
    im = Map.fromList [((x,y),0) | x <- [0..999], y <- [0..999]]
    eval (TurnOn rs) = Map.unionWith (+) (Map.fromList $ fmap (,1) (expand rs))
    eval (TurnOff rs) = Map.unionWith (\a b -> max 0 (a + b)) (Map.fromList $ fmap (,-1) (expand rs))
    eval (Toggle rs) = Map.unionWith (+) (Map.fromList $ fmap (,2) (expand rs))
