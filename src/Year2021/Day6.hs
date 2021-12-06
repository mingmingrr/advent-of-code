{-# LANGUAGE CPP #-}

module Year2021.Day6 where

import Util
import System.FilePath

part1, part2 :: Int
part1 = 80
part2 = 256

main = readFile (replaceExtension __FILE__ ".in") >>= print . sum
  . (!! part2) . iterate (\(x:xs) -> zipWith (+) [0,0,0,0,0,0,x,0,x] (xs++[0]))
  . (\m -> map (m !??) [0..8]) . counter . readOnlyNums

