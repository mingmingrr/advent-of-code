{-# LANGUAGE CPP #-}

module Year2024.Day13 where

import Util
import Linear.V2
import Linear.Matrix
import Data.Ratio
import Data.List.Split
import System.FilePath

part1, part2 :: Num a => a
part1 = 0
part2 = 10000000000000

solve :: [Rational] -> Integer
solve ns@[x1, y1, x2, y2, x3, y3] = case (denominator a, denominator b) of
  (1, 1) -> 3 * numerator a + numerator b ; (_, _) -> 0
  where V2 a b = inv22 (V2 (V2 x1 x2) (V2 y1 y2)) !* (V2 x3 y3 + part2)

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . sum . map solve . chunksOf 6 . map fromInteger . readOnlyNums

