{-# LANGUAGE CPP #-}

module Year2021.Day12 where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split
import System.FilePath

part1, part2 :: Bool
part1 = False
part2 = True

paths :: Map String [String] -> Int
paths grid = g Nothing [] "start" where
  g _ _ "end" = 1
  g double seen posn = sum
    [ g (if dupe then Just p else double) (posn : seen) p
    | p <- grid Map.! posn \\ ["start"]
    , let dupe = isLower (head p) && p `elem` seen
    , not dupe || part2 && isNothing double ]

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . paths . Map.fromListWith (++) .
    (lines >=> \xs -> let [x,y] = splitOn "-" xs in [(x,[y]),(y,[x])])
