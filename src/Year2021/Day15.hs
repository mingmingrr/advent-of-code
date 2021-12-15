{-# LANGUAGE CPP #-}

module Year2021.Day15 where

import Util
import Safe
import Linear.V2
import System.FilePath
import Control.Monad
import Data.Char
import qualified Data.Map as Map

part1, part2 :: Int
part1 = 1
part2 = 5

main = readFile (replaceExtension __FILE__ ".in") >>= \input ->
  let grid = Map.map digitToInt . Map.fromList . labelGrid $ lines input
      high = snd (bounds (Map.keys grid)) + 1
      grid' = Map.unions
        [ Map.mapKeysMonotonic (+ offset * (fst (Map.findMax grid) + 1))
        $ Map.map (succ . flip mod 9 . pred . (+ sum offset)) grid
        | offset <- join (liftM2 V2) [0..part2-1] ]
      neigh n = [(v, d) | d <- (+ n) <$> adjacent, Just v <- [grid' Map.!? d]]
   in print . fst . findJust ((== fst (Map.findMax grid')) . head . snd) $ dijkstra neigh 0

