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
  print . Map.size . Map.filter (> 1) $ counter [ x + fromIntegral n * signum d
    | [x, y] <- map (map (uncurry' V2) . sort . chunksOf 2 . readOnlyNums) (lines input)
    , let d = y - x, part2 || 0 `elem` d, n <- [0..maximum d] ]

