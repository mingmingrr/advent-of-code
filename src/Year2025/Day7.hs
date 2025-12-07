{-# LANGUAGE CPP #-}

module Year2025.Day7 where

import Util
import Data.List
import System.FilePath
import qualified Data.Map as Map

part1 :: Map.Map Int Int -> Int 
part1 = (Map.! (-2))
part2 = liftA2 (-) (sum . Map.elems) part1

run :: Map.Map Int Int -> String -> Map.Map Int Int
run ns xs = Map.fromListWith (+) ((-2, length ns' - Map.size ns) : ns') where
  ns' = [(d, c) | (n, c) <- Map.toList ns, d <- if xs !? n == Just '^' then [n - 1, n + 1] else [n]]

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part2 . maybe undefined (uncurry (foldl' run . counter . elemIndices 'S')) . uncons . lines
