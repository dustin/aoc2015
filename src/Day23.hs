{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day23 where

import           Advent.AoC                 (Parser, final, parseFile)
import           Control.Applicative        ((<|>))
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Text.Megaparsec            (endBy)
import           Text.Megaparsec.Char       (space)
import qualified Text.Megaparsec.Char.Lexer as L

{-
hlf r sets register r to half its current value, then continues with the next instruction.
tpl r sets register r to triple its current value, then continues with the next instruction.
inc r increments register r, adding 1 to it, then continues with the next instruction.
jmp offset is a jump; it continues with the instruction offset away relative to itself.
jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).
-}

data Reg = RegA | RegB deriving (Show, Enum)

data Instruction = Half Reg
                 | Triple Reg
                 | Increment Reg
                 | Jump Int
                 | JIE Reg Int
                 | JIO Reg Int
  deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

getInput :: FilePath -> IO (Vector Instruction)
getInput = parseFile (V.fromList <$> instruction `endBy` "\n")
  where instruction = hlf <|> tpl <|> inc <|> jmp <|> jie <|> jio
        hlf = Half <$> regop "hlf"
        tpl = Triple <$> regop "tpl"
        inc = Increment <$> regop "inc"
        jmp = lexeme "jmp" *> (Jump <$> offset)
        jie = lexeme "jie" *> (JIE <$> reg <* lexeme "," <*> offset)
        jio = lexeme "jio" *> (JIO <$> reg <* lexeme "," <*> offset)
        reg :: Parser Reg
        reg = RegA <$ "a" <|> RegB <$ "b"
        offset :: Parser Int
        offset = L.signed space L.decimal
        regop n = lexeme n *> reg

data Machine = Machine {
  _regA :: Int,
  _regB :: Int,
  _ins  :: Vector Instruction
  } deriving Show

readReg :: Reg -> Machine -> Int
readReg RegA = _regA
readReg RegB = _regB

eval :: Machine -> Int -> Maybe (Machine, Int)
eval m@Machine{..} off = e <$> _ins V.!? off
    where
      e (Half r)                       = modifyReg r (`div` 2)
      e (Triple r)                     = modifyReg r (* 3)
      e (Increment r)                  = modifyReg r succ
      e (Jump o)                       = (m, off + o)
      e (JIE r o) | even (readReg r m) = (m, off + o)
      e (JIO r o) | readReg r m == 1   = (m, off + o)
      e _                              = (m, off + 1)

      modifyReg RegA f = (m{_regA = f _regA}, off+1)
      modifyReg RegB f = (m{_regB = f _regB}, off+1)

part1 :: IO Int
part1 = do
  ins <- getInput "input/day23"
  pure $ readReg RegB . fst $ final (uncurry eval) (Machine 0 0 ins, 0)

part2 :: IO Int
part2 = do
  ins <- getInput "input/day23"
  pure $ readReg RegB . fst $ final (uncurry eval) (Machine 1 0 ins, 0)
