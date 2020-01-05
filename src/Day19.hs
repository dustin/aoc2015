{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module Day19 where

import           Advent.AoC                 (parseFile)
import           Control.Applicative        (liftA2)
import           Data.Char                  (isUpper)
import           Data.List                  (isPrefixOf)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Text.Internal.Search  as T
import           Text.Megaparsec            (many, try)
import           Text.Megaparsec.Char       (eol, letterChar, space, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Replacements = Map String [String]

getInput :: FilePath -> IO (Replacements, String)
getInput fp = parseFile parseAll fp
  where parseAll  = liftA2 (,) (Map.fromListWith (<>) <$> many (try parseRepl)) (space1 *> many letterChar)
        parseRepl = liftA2 (,) (lexeme (many letterChar)) (lexeme "=>" *> ((:[]) <$> many letterChar) <* eol)
        lexeme = L.lexeme space

expand :: Replacements -> String -> [String]
expand m = nf
  where
    ks = Map.keys m
    nf [] = []
    nf s@(x:xs) = foldMap prefixed ks <> fmap (x:) (nf xs)
      where
        prefixed k | k `isPrefixOf` s = map (<> rest) (m Map.! k)
          where rest = drop (length k) s
        prefixed _ = []

part1 :: IO Int
part1 = length . Set.fromList . uncurry expand <$> getInput "input/day19"

compute :: String -> Int
compute s = uppers - count "Ar" - count "Rn" - (2 * count "Y") - 1
  where
    st = T.pack s
    uppers = T.length . T.filter isUpper $ st
    count = length . flip T.indices st

part2 :: IO Int
part2 = compute . snd <$> getInput "input/day19"
