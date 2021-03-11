{-# LANGUAGE CPP #-}

module Year2020.Day10 where

import Year2020.Util

import Data.List

import System.FilePath

tribonacci :: Num a => [a]
tribonacci = 0 : 0 : 1 : zipWith3 (\x y z -> x + y + z)
  tribonacci (tail tribonacci) (drop 2 tribonacci)

difference :: Num a => [a] -> [a]
difference = zipWith (-) <$> tail <*> id

countStep :: (Num a, Ord a) => a -> [a] -> a
countStep n = fromIntegral . length . filter (== n) . difference

part1, part2 :: [Integer] -> Integer
part1 = ((*) <$> succ . countStep 3 <*> countStep 1) . (0:)
part2 = product . map ((tribonacci !!) . (+ 2) . length)
      . filter ((== 1) . head) . group . difference . (0:)

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . part2 . sort . map readIntegral $ lines input

