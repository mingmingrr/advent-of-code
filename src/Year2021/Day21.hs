{-# LANGUAGE CPP #-}

module Year2021.Day21 where

import Util
import Data.Function.Memoize
import Control.Lens
import Control.Monad
import System.FilePath

part1, part2 :: Int -> Int -> Int
part1 a b = run 0 (a,0) (b,0) where
  run d (p1, s1) (p2, s2) | s2 >= 1000 = s1 * d
  run d (p1, s1) n2 = run (d + 3) n2 (p', s1 + p') where
    p' = mod (p1 + 3 * d + 5) 10 + 1
part2 a b = maximum1Of each (wins (a,0) (b,0)) where
  wins = memoize2 run
  run n1 (p2, s2) | s2 >= 21 = (0, 1)
  run (p1, s1) n2 = foldr collect (0,0) (replicateM 3 [1..3]) where
    collect dice (w, l) = (w + w', l + l') where
      p' = mod (p1 + sum dice - 1) 10 + 1
      (l', w') = wins n2 (p', s1 + p')

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . uncurry' part1 . map (last . readOnlyNums) . lines

