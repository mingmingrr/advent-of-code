{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Year2022.Day1 where

import Util
import Data.Ord
import Data.List
import System.FilePath

part1, part2 :: [Int] -> Int
part1 = head
part2 = sum . take 3

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part2 . sortOn Down . map (sum . map (read @Int)) . paragraphs

