{-# LANGUAGE CPP #-}

module Year2025.Day5 where

import Util
import Data.List
import System.FilePath

part1, part2 :: Int
part1 = 0
part2 = 1

merge :: [[Int]] -> [[Int]]
merge [x] = [x]
merge ([a,b]:[c,d]:xs) | b >= c = merge ([a, max b d] : xs)
merge (x:xs) = x : merge xs

main = map (map (readOnlyNums @Int)) . paragraphs <$> readFile (replaceExtension __FILE__ ".in") >>= \[xs, ys] ->
  print $ sum $ [ [fromEnum (or [a <= y && y <= b | [a, b] <- xs]) | [y] <- ys], [b - a + 1 | [a, b] <- merge (sort xs)] ] !! part2

