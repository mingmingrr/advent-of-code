{-# LANGUAGE CPP #-}

module Year2020.Day6 where

import Util

import System.FilePath

import qualified Data.Set as Set

part1, part2 :: [String] -> Int
part1 = Set.size . Set.fromList . concat
part2 = Set.size . foldr1 Set.intersection . map Set.fromList

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . sum . map part2 $ paragraphs input

