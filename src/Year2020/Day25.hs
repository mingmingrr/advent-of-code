{-# LANGUAGE CPP #-}

module Year2020.Day25 where

import System.FilePath

import Math.NumberTheory.Powers.Modular

part1 :: [Int] -> Int
part1 [x, y] = x ^% head (filter (\i -> powModInt 7 i 20201227 == y) [0..])

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . part1 . map read $ lines input

