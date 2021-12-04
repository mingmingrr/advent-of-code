{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Year2020.Day9 where

import Util
import Data.List
import Control.Monad
import System.FilePath

part1, part2 :: [Int] -> Int
part1 = maybe 0 last . find filt . transpose . take 26 . tails where
  filt (reverse -> x:xs) = all ((/= x) . sum) (replicateM 2 xs)
part2 xs@(part1 -> n) = maybe 0 ((+) <$> minimum <*> maximum)
  . find ((== n) . sum) . (tails <=< inits) $ takeWhile (/= n) xs

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . part2 $ readOnlyNums input

