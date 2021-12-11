{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Year2021.Day11 where

import Safe
import Numeric.LinearAlgebra
import System.FilePath

run :: Matrix Z -> Matrix Z
run grid = findJust ((<= 9) . maxElement) $ iterate f (grid + 1) where
  f grid = (grid + conv) * cond grid 0 1 0 1 * cond grid 9 1 1 0 where
    conv = conv2 (konst 1 (3, 3)) (cond grid 9 0 0 1)
      ?? (Drop 1, Drop 1) ?? (DropLast 1, DropLast 1)

part1, part2 :: [Matrix Z] -> Z
part1 = sum . map (sumElements . (1 -) . step) . take 100 . tail 
part2 = fromIntegral . findIndexJust ((== 0) . sumElements)

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part2 . iterate run . fromLists . map (map (read . pure)) . lines

