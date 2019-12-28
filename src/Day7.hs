{-# LANGUAGE LambdaCase #-}

module Day7 where

import           Advent.AoC                 (Parser, fst3, parseFile)
import           Control.Applicative        (liftA2, (<|>))
import           Data.Bits                  (complement, shift, (.&.), (.|.))
import qualified Data.Graph                 as G
import           Data.List                  (foldl')
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Word                  (Word16)
import           Text.Megaparsec            (endBy, many, try)
import           Text.Megaparsec.Char       (lowerChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

data Value = Literal Word16 | Wire String deriving Show

data Op = AND Value Value
        | SHIFT Value Int
        | OR Value Value
        | NOT Value
        | Direct Value deriving Show

data Inst = Inst Op String deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

getInput :: FilePath -> IO [Inst]
getInput fp = parseFile parseAll fp
  where parseAll :: Parser [Inst]
        parseAll = parseInst `endBy` "\n"
        parseInst :: Parser Inst
        parseInst = liftA2 Inst (lexeme parseOp) (lexeme "->" *> many lowerChar)
        parseValue :: Parser Value
        parseValue = Literal <$> L.decimal
                     <|> Wire <$> many lowerChar
        parseOp :: Parser Op
        parseOp = try parseAnd
                  <|> try parseRshift
                  <|> try parseLshift
                  <|> try parseOr
                  <|> try parseNot
                  <|> Direct <$> try parseValue

        parseAnd = AND <$> lexeme parseValue <*> (lexeme "AND" *> lexeme parseValue)
        parseOr = OR <$> lexeme parseValue <*> (lexeme "OR" *> lexeme parseValue)
        parseLshift = SHIFT <$> lexeme parseValue <*> (lexeme "LSHIFT" *> lexeme L.decimal)
        parseRshift = SHIFT <$> lexeme parseValue <*> (lexeme "RSHIFT" *> (negate <$> lexeme L.decimal))
        parseNot = NOT <$> (lexeme "NOT" *> lexeme parseValue)

eval :: [Inst] -> Map String Word16
eval ins = foldl' (flip e) mempty (reverse topSorted)
  where
    topSorted = let (g, v2n, _) = G.graphFromEdges . map (\(i, d, deps) -> (i, d, deps)) $ toDeps
                in fst3 . v2n <$> G.topSort g
    toDeps :: [(Inst, String, [String])]
    toDeps = map (\case
                     i@(Inst (Direct x) w)  -> (i, w, depsOf x)
                     i@(Inst (NOT x) w)     -> (i, w, depsOf x)
                     i@(Inst (AND w1 w2) w) -> (i, w, depsOf w1 <> depsOf w2)
                     i@(Inst (OR w1 w2) w)  -> (i, w, depsOf w1 <> depsOf w2)
                     i@(Inst (SHIFT x _) w) -> (i, w, depsOf x)
                 ) ins
    depsOf (Literal _) = []
    depsOf (Wire x)    = [x]

    e :: Inst -> Map String Word16 -> Map String Word16
    e (Inst (Direct x) w)  m = Map.insert w (deref x m) m
    e (Inst (NOT n) w)     m = Map.insert w (complement $ deref n m) m
    e (Inst (AND w1 w2) w) m = Map.insert w (deref w1 m .&. deref w2 m) m
    e (Inst (OR w1 w2) w)  m = Map.insert w (deref w1 m .|. deref w2 m) m
    e (Inst (SHIFT n i) w) m = Map.insert w (deref n m `shift` i) m

    deref :: Value -> Map String Word16 -> Word16
    deref (Literal x) = const x
    deref (Wire x)    = (Map.! x)

part1 :: IO Word16
part1 = (Map.! "a") . eval <$> getInput "input/day7"


part2 :: IO Word16
part2 = do
  ins <- getInput "input/day7"
  let p1 = eval ins Map.! "a"
  pure $ eval ((Inst (Direct (Literal p1)) "b"):ins) Map.! "a"
