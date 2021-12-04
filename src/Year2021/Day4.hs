{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day4 where

import Util
import Data.List
import Data.List.Extra (minimumOn, maximumOn)
import Data.List.Split
import Control.Lens
import System.FilePath

part1, part2 :: Ord b => (a -> b) -> [a] -> a
part1 = maximumOn
part2 = minimumOn

play :: [Int] -> [[(Int, Int)]] -> (Int, Int)
play (x:xs) board = if any (all ((== 1) . fst)) ((<*>) (++) transpose board)
  then (length xs + 1, sum (map (uncurry ((*) . (1-))) (concat board)))
  else play xs (set (mapped . mapped . filtered (views _2 (== x)) . _1) 1 board)

main = readFile (replaceExtension __FILE__ ".in") >>= \xs ->
  let (ns@(reverse -> ms) : bs) = map readOnlyNums (lines xs)
   in print . uncurry ((*) . (ms !!)) . part2 fst
        . map (play ns . map (map (0,)) . tail) $ chunksOf 6 bs

