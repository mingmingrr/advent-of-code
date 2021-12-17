{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day17 where

import Util
import Linear.V2
import Data.List
import Control.Lens
import System.FilePath

path :: V2 Int -> [V2 Int]
path = unfoldr (Just . next) . (0,) where
  next (p, v@(V2 x y)) = (p, (p + v, V2 (x - signum x) (y + 1)))

part1, part2 :: [[V2 Int]] -> Int
part1 = maybe undefined negate . minimumOf (folded . folded . _y)
part2 = length

main = readFile (replaceExtension __FILE__ ".in") >>=
  \(readOnlyNums -> [x1,x2,y2,y1]) -> print $ part1 [ xs
    | v <- V2 <$> [1..x2] <*> [4*(-y2)..y2]
    , let xs = takeWhile ((<= y2) . (^. _y)) (path v)
    , or [x1 <= a && a <= x2 && y1 <= b && b <= y2 | V2 a b <- xs] ]

