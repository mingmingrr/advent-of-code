{-# LANGUAGE CPP #-}

module Year2024.Day1 where

import Util
import qualified Data.Map as Map
import Data.List
import System.FilePath

part1, part2 :: [[Int]] -> [Int]
part1 = uncurry' (zipWith ((abs .) . (-)))
part2 = Map.elems . uncurry' (Map.intersectionWithKey (((*) .) . (*))) . map counter

main = readFile (replaceExtension __FILE__ ".in") >>= print
  . sum . part2 . map sort . transpose . map readOnlyNums . lines

