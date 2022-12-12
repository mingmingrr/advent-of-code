{-# LANGUAGE CPP #-}

module Year2022.Day12 where

import Util
import Linear.V2
import System.FilePath
import Data.Map (Map)
import qualified Data.Map as Map

neigh :: Map (V2 Int) Char -> V2 Int -> [(Int, V2 Int)]
neigh grid p = map ((,) 1) (neighbors adjacent
  [ maybe False ((>= elev (grid Map.! p) - 1) . elev) . flip Map.lookup grid ] p)
  where elev x = fromEnum . maybe x id $ lookup x [('S', 'a'), ('E', 'z')]

part1, part2 :: [Char]
part1 = "S"
part2 = "Sa"

main = Map.fromList . labelGrid . lines <$> readFile (replaceExtension __FILE__ ".in")
  >>= \grid -> print . minimum . map fst . filter ((`elem` part2) . (grid Map.!) . head . snd)
             $ dijkstra (neigh grid) =<< Map.keys (Map.filter (== 'E') grid)

