{-# LANGUAGE CPP #-}

module Year2022.Day15 where

import Util
import Linear.V2
import Linear.Matrix
import Data.List
import Data.List.Extra (groupOn)
import Data.List.Split
import Control.Lens
import System.FilePath

part1, part2 :: [(V2 Int, V2 Int)] -> Int
part1 sensors = subtract dupes . sum . map (succ . uncurry subtract) . squish $ sort
  [ (x - w, x + w) | (a@(V2 x y), b) <- sensors
  , let w = sum (abs (a - b)) - abs (y - 2000000) , w >= 0 ]
  where dupes = length $ nub [ b | (_, b@(V2 _ 2000000)) <- sensors ]
part2 sensors = gap [] . groupOn (^. _1) . sort $ concat
  [ [(x - d, ys, False), (x + d, ys, True)]
  | (a, b) <- sensors, let d = sum (abs (a - b))
  , let V2 x y = V2 (V2 1 (-1)) 1 !* a, let ys = (y - d, y + d) ]
  where
    gap ns (rs@((x,_,_):_):xs) = case squish ns' of
      ((_,y):_:_) -> sum (V2 4000000 (V2 (-1) 1) !* (V2 x y + 1)) `div` 2
      _ -> gap ns' xs
      where ns' = foldr (\(_,y,b) -> if b then delete y else insert y) ns rs

squish :: [(Int, Int)] -> [(Int, Int)]
squish ((a,b):(c,d):xs) = if b + 1 >= c
  then squish ((a,max b d):xs)
  else (a,b) : squish ((c,d):xs)
squish xs = xs

main = readFile (replaceExtension __FILE__ ".in") >>= print . part2
  . map (tuplify . map (uncurry' V2) . chunksOf 2 . readOnlyNums') . lines
