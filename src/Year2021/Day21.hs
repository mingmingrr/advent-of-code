{-# LANGUAGE CPP #-}

module Year2021.Day21 where

import Util
import Data.Function.Memoize
import Control.Lens
import Control.Monad
import System.FilePath

(<+>) :: Int -> (Int, Int) -> (Int, Int)
dice <+> (posn, score) = (p, score + p) where
  p = mod (posn + dice - 1) 10 + 1

part1, part2 :: Int -> Int -> Int
part1 a b = run 0 (a,0) (b,0) where
  run d (_, s1) (_, s2) | s2 >= 1000 = s1 * d
  run d n1 n2 = run (d + 3) n2 ((3 * d + 6) <+> n1)
part2 a b = maximum1Of each (wins (a,0) (b,0)) where
  wins = memoize2 run
  run n1 (p2, s2) | s2 >= 21 = (0, 1)
  run n1 n2 = foldr collect (0,0) (replicateM 3 [1..3]) where
    collect dice (w, l) = (w + w', l + l') where
      (l', w') = wins n2 (sum dice <+> n1)

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . uncurry' part2 . map (last . readOnlyNums) . lines

