{-# LANGUAGE CPP #-}

module Year2021.Day5 where

import Util
import Linear.V2
import Data.List
import Data.List.Split
import System.FilePath
import qualified Data.Map as Map

part1, part2 :: Bool
part1 = False
part2 = True

main = readFile (replaceExtension __FILE__ ".in") >>= \input ->
  print . Map.size . Map.filter (> 1) $ counter
    [ V2 (a + n * signum (c - a)) (b + n * signum (d - b))
    | [[a,b],[c,d]] <- map (sort . chunksOf 2 . readOnlyNums) (lines input)
    , part2 || a == c || b == d, n <- [0..max (c - a) (d - b)] ]

