{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Year2022.Day18 where

import Util
import Linear
import Linear.V3
import System.FilePath
import Data.Set (Set)
import qualified Data.Set as Set

part1, part2 :: [[V3 Int]] -> (Bool, [V3 Int])
part1 = (,) False . head
part2 = (,) True . (!! 1)

main = readFile (replaceExtension __FILE__ ".in") >>= \input ->
  let cubes = Set.fromList . map (uncurry3' V3 . readOnlyNums) $ lines input
      (subtract 1 -> minP, (+ 1) -> maxP) = bounds cubes
      neigh b = map ((,) 1) . neighbors (basis ++ map negate basis)
        [ \p -> inBounds minP maxP p && Set.member p cubes == b ]
      (inside, points) = part2 [ Set.toList cubes
        , map (head . snd) $ dijkstra (neigh False) minP ]
   in print . length $ concatMap (neigh inside) points

