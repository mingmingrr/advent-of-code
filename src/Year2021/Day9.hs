{-# LANGUAGE CPP #-}

module Year2021.Day9 where

import Util
import Linear.V2
import Data.List
import Data.Ord
import Data.Char
import System.FilePath
import Data.Map (Map)
import qualified Data.Map as Map

part1, part2 :: Map (V2 Int) Char -> Int
part1 canyon = sum [digitToInt v + 1
  | (k, v) <- Map.toList canyon
  , and [maybe True (> v) (canyon Map.!? (k + d)) | d <- adjacent] ]
part2 canyon = product . take 3 . sortOn Down
  . run . Map.keys $ Map.filter (/= '9') canyon
  where run [] = []
        run (n:ns) = length visit : run (ns \\ visit)
          where visit = map (head . snd) (dijkstra neigh n)
                neigh p = [(1, n) | n <- map (+p) adjacent, n `elem` ns]

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part2 . Map.fromList . labelGrid . lines

