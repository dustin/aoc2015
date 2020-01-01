{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module Day19 where

import           Advent.AoC                 (parseFile)
import           Control.Applicative        (liftA2)
import           Data.Char                  (isUpper)
import           Data.List                  (isPrefixOf)
import           Data.List.Extra            (breakOn, repeatedly)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
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

-- var num = str.Count(char.IsUpper) - countStr("Rn") - countStr("Ar") - 2 * countStr("Y") - 1;
part2 :: IO Int
part2 = do
  (_, s) <- getInput "input/day19"
  let uppers = length . filter isUpper $ s
      count x = pred . length . repeatedly (fmap (drop (length x)) . breakOn x) $ s
      ys = count "Y"
      rns = count "Rn"
      ars = count "Ar"
  pure $ uppers - rns - ars - (2*ys) - 1
