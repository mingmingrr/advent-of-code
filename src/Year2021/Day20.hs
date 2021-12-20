{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day20 where

import Util
import Numeric.Bool
import Linear.V2
import Data.List
import Data.Bits
import System.FilePath
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

part1, part2 :: Int
part1 = 2
part2 = 50

life :: [Bool] -> (Bool, Set (V2 Int)) -> (Bool, Set (V2 Int))
life rules (parity, grid) = (xor (head rules) parity,
  Set.fromList [ x | x <- V2 <$> [a-1..c+1] <*> [b-1..d+1], rules !! readBase 2
    [Set.member p grid || parity && not (inBounds l h p) | p <- (+x) <$> vicinity] ])
  where (l@(V2 a b), h@(V2 c d)) = bounds grid
        vicinity = sort (adjacent <> diagonal <> [0])

main = readFile (replaceExtension __FILE__ ".in") >>= \(lines -> (rules:_:grid)) ->
  print . Set.size . snd $ iterate (life (map (=='#') rules))
    (False, Map.keysSet . Map.filter (=='#') . Map.fromList $ labelGrid grid) !! part2

