module Day15 where

import           Advent.AoC                 (Parser, parseFile)
import           Control.Applicative        (liftA2)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Monoid                (Product (..), Sum (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec            (endBy, many, sepBy)
import           Text.Megaparsec.Char       (letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

data Ing = Ing {
  name  :: Text,
  props :: Map Text Int
  } deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

getInput :: FilePath -> IO [Ing]
getInput = parseFile parseAll
  where
    parseAll = parseIng `endBy` "\n"
    parseIng = Ing <$> lexeme parseId <*> (lexeme ":" *> parseProps)
    parseId = T.pack <$> lexeme (many letterChar)
    parseProps = Map.fromList <$> parseProp `sepBy` lexeme ","
    parseProp = liftA2 (,) (lexeme parseId) (L.signed space L.decimal)

allProps :: [Text]
allProps = ["capacity", "durability", "flavor", "texture"]

type PropMap = Map Text (Map Text Int)

ingMap :: [Ing] -> PropMap
ingMap = Map.fromList . map (\Ing{..} -> (name, props))

scale :: Map Text Int -> PropMap -> PropMap
scale s = Map.mapWithKey (\k a -> fmap (* (s Map.! k)) a)

score :: Map Text Int -> PropMap -> (Int,Int)
score s m = (scores, cals)
  where
    scaled = scale s m
    scores = getProduct $ foldMap (\p -> Product . max 0 . getSum . foldMap (
                                      \pm -> Sum $ Map.findWithDefault 0 p pm ) $ scaled) allProps
    cals = getProduct $ foldMap (\p -> Product . max 0 . getSum . foldMap (
                                      \pm -> Sum $ Map.findWithDefault 0 p pm ) $ scaled) ["calories"]

gen4Hundies :: [[Int]]
gen4Hundies = [ [a, b, c, d]
              | a <- [100, 99 .. 1],
                b <- [a - 1, a - 2 .. 1],
                c <- [b - 1, b - 2 .. 1],
                d <- [100 - (a+b+c)],
                d > 0]

part1 :: IO Int
part1 = do
  m <- ingMap <$> getInput "input/day15"
  let ks = Map.keys m
      scores = map (\hs -> score (Map.fromList (zip ks hs)) m) gen4Hundies
  pure . maximum . fmap fst $ scores

part2 :: IO Int
part2 = do
  m <- ingMap <$> getInput "input/day15"
  let ks = Map.keys m
      scores = map (\hs -> score (Map.fromList (zip ks hs)) m) gen4Hundies
  pure . maximum . fmap fst . filter ((== 500) . snd) $ scores

