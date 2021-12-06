{-# LANGUAGE CPP #-}

module Year2021.Day6 where

import Util
import Data.List
import System.FilePath

part1, part2 :: Int
part1 = 80
part2 = 256

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . sum .  map ((drop part2 fish !!) . (8 -)) . readOnlyNums
  where fish = replicate 9 1 ++ zipWith (+) fish (tail (tail fish))

