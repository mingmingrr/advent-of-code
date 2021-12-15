{-# LANGUAGE CPP #-}

module Year2021.Day15 where

import Numeric.LinearAlgebra
import Util
import Safe
import Linear.V2
import System.FilePath
import Data.Bifunctor

part1, part2 :: Int
part1 = 1
part2 = 5

main = readFile (replaceExtension __FILE__ ".in") >>= \input ->
  let grid = fromLists . map (map (read . pure)) $ lines input :: Matrix Z
      grid' = cmod 9 (fromBlocks [[grid + scalar (r + c) | c <- [0..4]] | r <- [0..4]] - 1) + 1
      neigh (a, b) = [ (grid' ! x ! y, (x, y))
        | p@(V2 x y) <- (+ V2 a b) <$> adjacent
        , inBounds 0 (uncurry V2 (size grid') - 1) p ]
   in print . fst . findJust ((== size grid') . bimap succ succ . head . snd)
        $ dijkstra neigh (0, 0)

