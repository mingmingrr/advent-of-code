{-# LANGUAGE CPP #-}

module Year2021.Day1 where

import Year2020.Util
import Data.List
import System.FilePath

part1, part2 :: Int
part1 = 1
part2 = 3

main = readFile (replaceExtension __FILE__ ".in") >>= print
  . length . filter id . (zipWith (<) <*> tail)
  . map sum . transpose . take part2 . tails
  . map readInteger . lines

