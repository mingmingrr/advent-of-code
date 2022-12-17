{-# LANGUAGE CPP #-}

module Year2022.Day17 where

import Util
import Linear.V2
import Data.List
import Control.Lens
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath

type Rock = Set (V2 Int)
type Field = Rock

rocks :: [(Int, Rock)]
rocks = cycle . zip [0..] $ map Set.fromList [
  map (V2 0) [2..5], -- -
  [V2 0 3, V2 1 2, V2 1 3, V2 1 4, V2 2 3], -- +
  [V2 0 2, V2 0 3, V2 0 4, V2 1 4, V2 2 4], -- J
  map (`V2` 2) [0..3], -- I
  V2 <$> [0..1] <*> [2..3]] -- O

fall :: Field -> [(Int, Rock)] -> [(Int, Int)] -> [((Int, Int), Int)]
fall field ((rn,rock):rocks) winds@((wn,_):_)
  = ((rn, wn), Set.findMax field ^. _x) : fall field' rocks winds'
  where (field', winds') = fall1 field rock' winds
        rock' = Set.mapMonotonic (_x +~ (4 + (Set.findMax field ^. _x))) rock

fall1 :: Field -> Rock -> [(Int, Int)] -> (Field, [(Int, Int)])
fall1 field rock ((_,wind):winds) =
  if Set.null (Set.intersection fallen field)
    then fall1 field fallen winds
    else (Set.union field winded, winds)
  where moved = Set.mapMonotonic (_y +~ wind) rock
        winded | not $ all (between 0 6 . (^. _y)) moved = rock
               | not . null $ Set.intersection moved field = rock
               | otherwise = moved
        fallen = Set.mapMonotonic (_x -~ 1) winded

part1, part2 :: [((Int, Int), Int)] -> Int
part1 = snd . (!! 2022)
part2 xs = d * (snd (xs !! (500 + l)) - snd (xs !! 500)) + snd (xs !! (500 + m))
  where (d, m) = divMod (1000000000000 - 500) l
        l = (!! 1) . (elemIndices =<< head) . drop 500 $ map fst xs

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part2 . fall (Set.fromList (map (V2 0) [0..6])) rocks
    . cycle . zip [0..] . map (subtract 61 . fromEnum) . filter (`elem` "<>")
