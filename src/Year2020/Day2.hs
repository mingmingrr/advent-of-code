{-# LANGUAGE CPP #-}

module Year2020.Day2 where

import Util

import Data.Bits
import Data.Maybe
import Data.List.Split

import System.FilePath

import qualified Data.Map as Map

part1, part2 :: [String] -> Bool
part1 [a, b, [c], _, s] = read a <= x && x <= read b
   where x = fromMaybe 0 (counter s Map.!? c)
part2 [a, b, [c], _, s] = xor (s !! (read a - 1) == c) (s !! (read b - 1) == c)

main = readFile (replaceExtension __FILE__ ".in") >>=
   print . length . filter part2 . map (splitOneOf ":- ") . lines
