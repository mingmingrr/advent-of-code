{-# LANGUAGE CPP #-}

module Year2025.Day6 where

import Util
import Safe
import Data.List
import Data.List.Split
import System.FilePath

part1, part2 :: [String] -> [String]
part1 = id
part2 = transpose

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . sum . map (maybe 0 calc . unsnoc . transpose) . splitWhen ((== " ") . nub) . transpose . lines
  where calc (nums, op) = foldl1' (lookupJust (head (filter (/= ' ') op)) operators) (map read (part2 nums))

