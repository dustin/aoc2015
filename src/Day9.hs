module Day9 where

import           Advent.AoC                 (Parser, parseFile)
import           Advent.Search              (bfs, dijkstra)
import           Data.List.Extra            (maximumOn)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec            (endBy, many)
import           Text.Megaparsec.Char       (letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

data Route = Route !Text !Text !Int deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

getInput :: FilePath -> IO [Route]
getInput fp = parseFile parseAll fp
  where parseAll = parseRoute `endBy` "\n"
        parseRoute = Route <$> lexeme loc <*> (lexeme "to" *> lexeme loc) <*> (lexeme "=" *> L.decimal)
        loc = T.pack <$> many letterChar

mkMap :: [Route] -> Map Text [(Int, Text)]
mkMap = Map.fromListWith (<>) . foldMap (\(Route f t c) -> [(t,[(c,f)]), (f,[(c,t)])])

allDests :: [Route] -> Set Text
allDests = Set.fromList . map (\(Route _ t _) -> t)

data RState = RState {
  curLoc :: Text,
  seen   :: Set Text
  } deriving (Show, Ord, Eq)

findPath :: [Route] -> Maybe (Int, [Text])
findPath rs = (fmap . fmap . fmap) curLoc $ dijkstra nf (RState "" mempty) ((== everywhere) . seen)
  where
    m = mkMap rs
    everywhere = Map.keysSet m
    nf RState{curLoc=""} = [(0,RState k (Set.singleton k)) | k <- Map.keys m]
    nf r@RState{..} = [(c, r{curLoc=k, seen=Set.insert k seen})
                      | (c,k) <- Map.findWithDefault [] curLoc m,
                        k `Set.notMember` seen]

data RState2 = RState2 {
  curLoc2 :: Text,
  seen2   :: Set Text,
  cost    :: Int
  } deriving (Show, Ord, Eq)

bfsPath :: [Route] -> [RState2]
bfsPath rs = filter ((== everywhere) . seen2) $ bfs nf (RState2 "" mempty 0)
  where
    m = mkMap rs
    everywhere = Map.keysSet m
    nf RState2{curLoc2=""} = [RState2 k (Set.singleton k) 0 | k <- Map.keys m]
    nf r@RState2{..} = [r{curLoc2=k, seen2=Set.insert k seen2, cost=cost+c}
                       | (c,k) <- Map.findWithDefault [] curLoc2 m,
                         k `Set.notMember` seen2]

part1 :: IO (Maybe Int)
part1 = fmap fst . findPath <$> getInput "input/day9"

part2' :: [Route] -> Int
part2' = cost . maximumOn cost . bfsPath

part2 :: IO Int
part2 = part2' <$> getInput "input/day9"
