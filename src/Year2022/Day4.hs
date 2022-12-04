{-# LANGUAGE CPP #-}

module Year2022.Day4 where

import Util
import Data.List
import System.FilePath

part1, part2 :: [Int] -> [Int] -> Bool
part1 xs ys = null (xs \\ ys) || null (ys \\ xs)
part2 xs ys = not . null $ xs `intersect` ys

main = readFile (replaceExtension __FILE__ ".in") >>= print . length
  . filter (\[a,b,c,d] -> part2 [a..b] [c..d]) . map readOnlyNums . lines

