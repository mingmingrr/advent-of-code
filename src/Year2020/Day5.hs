{-# LANGUAGE CPP #-}

module Year2020.Day5 where

import Util

import System.FilePath

import qualified Data.Set as Set

translate :: Char -> Char
translate 'F' = '0'
translate 'B' = '1'
translate 'L' = '0'
translate 'R' = '1'

part1, part2 :: [Int] -> Int
part1 = maximum
part2 xs = Set.findMax $ Set.fromList [0..maximum xs] Set.\\ Set.fromList xs

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . part2 . map (parseError pBinary . map translate) $ lines input

