{-# LANGUAGE CPP #-}

module Year2024.Day10 where

import Util
import Linear.V2
import System.FilePath
import Data.Map (Map)
import qualified Data.Map as Map

part1, part2 :: [Int] -> Int
part1 = length
part2 = sum

bfs :: Map (V2 Int) Char -> Char -> Map (V2 Int) Int -> [Int]
bfs gs '9' qs = Map.elems qs
bfs gs n qs = bfs gs (succ n) $ Map.fromListWith (+)
  [ (k', v) | (k, v) <- Map.toList qs , d <- orthogonal
  , let k' = k + d , gs Map.!? k' == Just (succ n) ]

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  let grid = Map.fromList . labelGrid $ lines input
  print $ sum $ map (part2 . bfs grid '0' . flip Map.singleton 1)
    $ Map.keys $ Map.filter (== '0') grid

