module Day14 where

import           Advent.AoC                 (Parser, parseFile)
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

deerDistance :: Int -> Deer -> Int
deerDistance n Deer{..} = min duration b * speed + a * dist
  where
    (a, b) = n `divMod` unit
    dist = speed * duration
    unit = duration + rest

part1 :: IO Int
part1 = maximum . map (deerDistance 2503) <$> getInput "input/day14"

part2 :: IO Int
part2 = pure 0
