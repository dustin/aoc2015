module Day14 where

import           Advent.AoC                 (Parser, parseFile)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec            (endBy, many)
import           Text.Megaparsec.Char       (letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

data Deer = Deer {
  name     :: Text,
  speed    :: Int,
  duration :: Int,
  rest     :: Int
  } deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

getInput :: FilePath -> IO [Deer]
getInput = parseFile parseAll
  where
    parseAll = parseDeer `endBy` "\n"
    parseDeer = Deer <$> parseName <*> parseSpeed <*> parseDuration <*> parseRest
    parseName = T.pack <$> lexeme (many letterChar) <* lexeme "can fly "
    parseSpeed = lexeme L.decimal <* lexeme "km/s for "
    parseDuration = lexeme L.decimal <* lexeme "seconds, but then must rest for "
    parseRest = lexeme L.decimal <* "seconds."

distance :: Int -> Deer -> Int
distance n Deer{..} = min duration b * speed + a * dist
  where
    (a, b) = n `divMod` unit
    dist = speed * duration
    unit = duration + rest

score :: [Deer] -> Int -> Map Text Int
score deer rounds = go mempty rounds
  where
    go m 0 = m
    go m n = go m' (n - 1)
      where
        speeds = [(distance n d, d) | d <- deer]
        winners = [d | (dist,d) <- speeds, dist == (maximum $ fst <$> speeds)]
        m' = Map.unionWith (+) (Map.fromList [(name winner, 1) | winner <- winners]) m

part1 :: IO Int
part1 = maximum . map (distance 2503) <$> getInput "input/day14"

part2 :: IO Int
part2 = do
  deer <- getInput "input/day14"
  pure $ maximum . Map.elems . score deer $ 2503
