{-# LANGUAGE CPP #-}

module Year2025.Day11 where

import Data.Function
import Linear.V4
import System.FilePath
import qualified Data.Map as Map

part1, part2 :: (String, [Int] -> Int)
part1 = ("you", sum)
part2 = ("svr", last)

walk :: [[String]] -> Map.Map String (V4 Int)
walk ns = fix $ \counts -> Map.fromList $ ("out", V4 1 0 0 0)
  : [ (x, reorder x $ foldr ((+) . (counts Map.!)) 0 xs) | x:xs <- ns ]

reorder :: String -> V4 Int -> V4 Int
reorder "dac" (V4 a b c d) = V4 0 (a + b) 0 (c + d)
reorder "fft" (V4 a b c d) = V4 0 0 (a + c) (b + d)
reorder _ r = r

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . summarize . foldr (:) [] . (Map.! source) . walk . map (words . filter (/= ':')) . lines
  where (source, summarize) = part2
