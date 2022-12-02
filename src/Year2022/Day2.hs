{-# LANGUAGE CPP #-}

module Year2022.Day2 where

import Numeric.Char
import Data.Char
import System.FilePath

part1, part2 :: Int -> Int -> Int
part1 x y = y + 1 + 3 * mod (y - x + 1) 3
part2 x y = part1 x (mod (x + y - 1) 3)

main = readFile (replaceExtension __FILE__ ".in") >>= \input ->
  print $ sum [ part1 (ord $ x - 'A') (ord $ y - 'X') | [x, _, y] <- lines input ]

