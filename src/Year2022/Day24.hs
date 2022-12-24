{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Year2022.Day24 where

import Util
import Linear.V2
import Data.Semigroup
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath

type Grid = Map (V2 Int) Char
type Blizz = Map (V2 Int) [V2 Int]
type Posn = Arg (Int, Int, V2 Int) [Blizz]

neigh :: Grid -> Posn -> [(Int, Posn)]
neigh grid (Arg (step, stage, posn) (blizz:blizzs)) =
  [ (1, Arg (step + 1, stage + fromEnum s, p) blizzs)
  | p <- neighbors (0 : orthogonal) [ flip Map.notMember blizz
    , maybe False (/= '#') . flip Map.lookup grid ] posn
  , let s = Map.notMember (p + V2 (if even stage then 1 else (-1)) 0) grid ]

move :: Grid -> Blizz -> Blizz
move grid blizz = Map.fromListWith (<>)
  [ (k', [v]) | (k, vs) <- Map.toList blizz , v <- vs
  , let k' = if Map.member (k + v + v) grid then k + v
          else until (`Map.notMember` grid) (subtract v) k + v + v ]

main = readFile (replaceExtension __FILE__ ".in") >>=
  \(Map.fromList . labelGrid . lines -> grid) -> print
    . fst . head . filter (\(_, Arg (_, s, p) _ : _) -> s == 3)
    . dijkstra (neigh grid) . Arg (0, 0, V2 0 1) . tail . iterate (move grid)
    . Map.map (:[]) $ Map.mapMaybe (compass Map.!?) grid
