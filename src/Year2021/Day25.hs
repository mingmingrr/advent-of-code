{-# LANGUAGE CPP #-}

module Year2021.Day25 where

import Util
import Linear.V2
import System.FilePath
import Data.Map (Map)
import qualified Data.Map as Map

run :: Int -> V2 Int -> Map (V2 Int) Char -> Int
run n limit xs = if xs == xs' then n else run (n + 1) limit xs' where
  xs' = xs `move` ('>', V2 0 1) `move` ('v', V2 1 0)
  move xs (c, d) = (xs Map.\\ Map.fromList as) <> Map.fromList bs where
    (as, bs) = unzip [ ((k, ()), (k', v))
      | (k, v) <- Map.toList xs , v == c
      , let k' = mod <$> (k + d) <*> limit
      , Map.notMember k' xs ]

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . (run 1 =<< (+ 1) . snd . bounds . Map.keys) .
    Map.filter (/= '.') . Map.fromList . labelGrid . lines

