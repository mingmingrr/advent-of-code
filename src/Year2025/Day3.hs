{-# LANGUAGE CPP #-}

module Year2025.Day3 where

import Data.Char
import System.FilePath

part1, part2 :: Int
part1 = 2
part2 = 12

generate :: Int -> [Int] -> [Int]
generate x ns = 0 : zipWith3 (\n a -> max (x * 10^n + a)) [0..] ns (tail ns ++ [0])

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . sum . map ((!! part2) . foldr (generate . digitToInt) [0]) . lines

