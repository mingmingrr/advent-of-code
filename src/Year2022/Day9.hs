{-# LANGUAGE CPP #-}

module Year2022.Day9 where

import Util
import Safe
import Linear.V2
import System.FilePath
import Data.List
import Data.Map ((!))

snek :: V2 Int -> V2 Int -> V2 Int
snek x y | all (< 2) (abs (x - y)) = y
snek x y = findJust (all (< 2) . abs . subtract x) . map (+ y) $
  if 0 `elem` (x - y) then adjacent else diagonal

part1, part2 :: Int
part1 = 2
part2 = 10

main = readFile (replaceExtension __FILE__ ".in")
  >>= print . length . nubSet . (!! pred part2) . transpose
    . scanl (\(n:ns) dir -> scanl snek (n + directions ! dir) ns) (repeat 0)
    . concatMap (uncurry' (flip (replicate . read)) . words) . lines

