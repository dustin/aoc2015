module Day13 where

import           Advent.AoC                 (Parser, parseFile)
import           Control.Applicative        ((<|>))
import           Data.List                  (permutations)
import           Data.List.Extra            (maximumOn)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Monoid                (Sum (..))
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec            (endBy, many, try)
import           Text.Megaparsec.Char       (char, letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

type Person = Text

data Happiness = Happiness !Person !Int !Person deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space


getInput :: FilePath -> IO [Happiness]
getInput = parseFile parseAll
  where
    parseAll = parseHappiness `endBy` "\n"
    parseHappiness = Happiness <$> lexeme person <*> lexeme amt <*> (hu *> person <* char '.')
    person = T.pack <$> lexeme (many letterChar)
    amt = try (lexeme "would gain") *> lexeme L.decimal
          <|> (lexeme "would lose") *> (negate <$> lexeme L.decimal)
    hu = lexeme "happiness units by sitting next to"

type HSMap = Map Person (Map Person Int)

hsMap :: [Happiness] -> HSMap
hsMap hs = Map.unionsWith (Map.union) [Map.singleton f (Map.singleton t c) | (Happiness f c t) <- hs]

people :: HSMap -> [Person]
people = Map.keys

type Arrangement = Seq Person

arrangements :: HSMap -> [Arrangement]
arrangements hs = Seq.fromList <$> permutations (people hs)

cost :: HSMap -> Arrangement -> Int
cost hs a = getSum . Seq.foldMapWithIndex score $ a
  where
    score n p = foldMap Sum (ncosts n p)
    ncosts n p = (\n' -> Map.findWithDefault 0 n' (Map.findWithDefault mempty p hs)) <$> neighbors n
    neighbors n = Seq.index a <$> ns n
    ns n
      | n == 0 = [1, length a - 1]
      | n == length a - 1 = [0, n - 1]
      | otherwise = [n-1,n+1]

bestArrangement :: HSMap -> (Int, Arrangement)
bestArrangement hs = maximumOn fst . fmap (\a -> (cost hs a, a)) . arrangements $ hs

part1 :: IO Int
part1 = fst . bestArrangement . hsMap <$> getInput "input/day13"

part2 :: IO Int
part2 = fst . bestArrangement . (Map.insert "Dustin" mempty) . hsMap <$> getInput "input/day13"
