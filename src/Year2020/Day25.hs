{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Year2020.Day25 where

import Util

import System.FilePath

import Data.Mod
import Math.NumberTheory.Moduli.Class

part1 :: [Int] -> Int
part1 [x, y] = fromIntegral . getVal $ z x ^% head [n | n <- [0..], 7 ^% n == z y]
  where z :: Int -> Mod 20201227
        z = fromIntegral

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part1 . map read . lines

