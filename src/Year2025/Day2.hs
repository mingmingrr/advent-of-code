{-# LANGUAGE CPP #-}

module Year2025.Day2 where

import Util
import Data.List.Split
import System.FilePath

part1, part2 :: Int -> Int -> [Int]
part1 = const . pure
part2 = enumFromTo

generate :: Int -> Int -> [Int]
generate x y = nubSet [ m
  | d <- part2 2 (length (show y))
  , n <- [chop d 0 x .. chop d 1 y]
  , let m = read $ concat $ replicate d $ show n
  , x <= m, m <= y ] where
  chop d n = read @Integer . ('0':) . (take =<< flip div d . (+) (d * n - n) . length) . show

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . sum . (>>= uncurry' generate) . chunksOf 2 . readOnlyNums

