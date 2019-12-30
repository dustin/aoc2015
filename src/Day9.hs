module Day9 where

import           Advent.AoC                 (Parser, parseFile)
import           Data.List                  (permutations)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Semigroup             (Max (..), Min (..))
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec            (endBy, many)
import           Text.Megaparsec.Char       (letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

data Route = Route !Text !Text !Int deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

type Costs = Map (Text,Text) Int

getInput :: FilePath -> IO Costs
getInput fp = costs <$> parseFile parseAll fp
  where parseAll = parseRoute `endBy` "\n"
        parseRoute = Route <$> lexeme loc <*> (lexeme "to" *> lexeme loc) <*> (lexeme "=" *> L.decimal)
        loc = T.pack <$> many letterChar

        costs = Map.fromList . foldMap (\(Route f t c) -> [((t,f),c),((f,t),c)])

most :: Monoid m => (Int -> m) -> Costs -> m
most f m = foldMap (f . cost) . permutations $ allDests
  where
    cost p = sum . zipWith (curry (m Map.!)) p $ tail p
    allDests = Set.toList . Set.fromList . map fst . Map.keys $ m

part1 :: Costs -> Int
part1 = getMin . most Min

part2 :: Costs -> Int
part2 = getMax . most Max
