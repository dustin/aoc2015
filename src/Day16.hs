module Day16 where

import           Advent.AoC                 (Parser, parseFile)
import           Control.Applicative        (liftA2)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec            (endBy, many, sepBy)
import           Text.Megaparsec.Char       (letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

type SueStuff = Map Text Int

data Sue = Sue {
  sueNum :: Int,
  stuff  :: SueStuff
  } deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

getInput :: FilePath -> IO (Seq Sue)
getInput fp = Seq.fromList <$> parseFile parseAll fp
  where
    parseAll = parseSue `endBy` "\n"
    parseSue = Sue <$> (lexeme "Sue" *> L.decimal) <*> (lexeme ":" *> parseProps)
    parseProps = Map.fromList <$> parseProp `sepBy` lexeme ","
    parseProp = liftA2 (,) ((T.pack <$> lexeme (many letterChar)) <* lexeme ":") L.decimal

want :: SueStuff
want = Map.fromList [("children", 3),
                     ("cats", 7),
                     ("samoyeds", 2),
                     ("pomeranians", 3),
                     ("akitas", 0),
                     ("vizslas", 0),
                     ("goldfish", 5),
                     ("trees", 3),
                     ("cars", 2),
                     ("perfumes", 1)
                    ]
part1 :: IO Int
part1 = do
  suz <- getInput "input/day16"
  let (sz Seq.:<| _) = Seq.filter (\Sue{stuff} -> Map.restrictKeys want (Map.keysSet stuff) == stuff) suz
  pure . sueNum $ sz

part2 :: IO Int
part2 = do
  suz <- getInput "input/day16"
  let (sz Seq.:<| _) = Seq.filter f suz
  pure . sueNum $ sz

  where f Sue{..} = baseCase && catCase && pomCase
          where baseCase = Map.restrictKeys want baseStuff == Map.restrictKeys stuff baseStuff
                baseStuff = Map.keysSet stuff `Set.difference` never

                catCase = gc 7 "cat" && gc 3 "trees"
                gc n a = fromMaybe True ((> n) <$> Map.lookup a stuff)

                pomCase = lc 3 "pomeranians" && lc 5 "goldfish"
                lc n a = fromMaybe True ((< n) <$> Map.lookup a stuff)
        never = Set.fromList ["cats", "trees", "pomeranians", "goldfish"]
