{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day14 where

import Util
import Data.List
import System.FilePath
import Data.Map (Map)
import qualified Data.Map as Map

run :: Int -> String -> Map String [String] -> Map String Int -> Int
run iters initpoly rules poly = flip div 2 . ((-) <$> maximum <*> minimum) . Map.elems
  . Map.unionWith (+) (Map.fromList [(head initpoly, 1), (last initpoly, 1)])
  $ Map.fromListWith (+) [(x,v) | (k, v) <- Map.toList (iterate iter poly !! iters), x <- k]
  where iter poly = Map.fromListWith (+) [(x, v) | (k, v) <- Map.toList poly, x <- rules Map.! k]

part1, part2 :: Int
part1 = 10
part2 = 40

main = readFile (replaceExtension __FILE__ ".in") >>= \(lines -> poly:_:rules) ->
  print $ run part2 poly
    (Map.fromList [([a,b],[[a,c],[c,b]]) | [[a,b],_,[c]] <- words <$> rules])
    (counter (transpose [init poly, tail poly]))

