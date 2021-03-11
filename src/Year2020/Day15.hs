{-# LANGUAGE NumericUnderscores #-}

module Year2020.Day15 where

import Data.List.Split

import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Storable.Mutable as VecSM

inner :: VecSM.MVector s Int -> Int -> Int -> Int -> ST s Int
inner vec 0 i n = pure n
inner vec t i n = do
  x <- VecSM.unsafeRead vec n
  VecSM.unsafeWrite vec n i
  inner vec (t - 1) (i + 1) (if x == 0 then 0 else i - x)

run :: Int -> [Int] -> Int
run n xs = runST $ do
  vec <- VecSM.new n
  zipWithM_ (VecSM.unsafeWrite vec) (init xs) [1..]
  inner vec (n - length xs) (length xs) (last xs)

part1, part2 :: Int
part1 = 2020
part2 = 30_000_000

main = do
  input <- getContents -- readFile (replaceExtension __FILE__ ".in")
  print . run part1 . map read . splitOn "," . head $ lines input

