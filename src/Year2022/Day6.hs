{-# LANGUAGE CPP #-}

module Year2022.Day6 where

import Safe
import Data.List
import System.FilePath

part1, part2 :: Int
part1 = 4
part2 = 14

main = readFile (replaceExtension __FILE__ ".in")
  >>= print . (+ n) . findIndexJust ((==) <*> nub)
    . transpose . take n . tails . head . lines
  where n = part2

