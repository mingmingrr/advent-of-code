{-# LANGUAGE CPP #-}

module Year2025.Day1 where

import Util
import Data.List.Extra (replace)
import System.FilePath

part1, part2 :: [Int] -> Int
part1 = length . filter ((== 0) . (`mod` 100))
part2 = sum . (zipWith f <*> tail) where
  f x y = let r = if x < y then 0 else 1 in abs (div (x - r) 100 - div (y - r) 100)

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part2 . scanl (+) 50 . readOnlyNums' @Int . replace "L" "-"

