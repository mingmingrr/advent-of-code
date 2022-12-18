{-# LANGUAGE CPP #-}

module Year2021.Day9 where

import Util
import Linear.V2
import Data.List
import Data.Ord
import Data.Char
import System.FilePath
import Data.Graph.Inductive

part1, part2 :: Gr (V2 Int, Char) (Char, Char) -> Int
part1 graph = sum [ digitToInt (snd (lab' ctx)) + 1
  | ctx <- contexts graph
  , all (uncurry (<) . edgeLabel) (out' ctx) ]
part2 = product . take 3 . sortOn Down . map length
  . scc . labfilter ((/= '9') . snd)

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part1 . snd . fromGrid orthogonal . lines

