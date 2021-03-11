{-# LANGUAGE CPP #-}

module Year2020.Day3 where

import Linear.V2

import Data.List.Split

import System.FilePath

tree n xs = if cycle xs !! n == '#' then 1 else 0
walk (V2 r d) = sum . zipWith tree [0,r..] . map head . chunksOf d

part1 = walk (V2 3 1)
part2 xs = product $ map (`walk` xs) [V2 1 1, V2 3 1, V2 5 1, V2 7 1, V2 1 3]

main = readFile (replaceExtension __FILE__ ".in") >>= print . part2 . lines

