{-# LANGUAGE CPP #-}

module Year2025.Day4 where

import Util
import Linear.V2
import System.FilePath
import qualified Data.Set as Set

part1, part2 :: [Int] -> Int
part1 = head
part2 = sum

forklift :: Set.Set (V2 Int) -> [Int]
forklift xs = if Set.null ys then [] else Set.size ys : forklift (xs `Set.difference` ys)
  where ys = Set.filter ((< 4) . length . neighbors (orthogonal <> diagonal) [flip Set.member xs]) xs

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part2 . forklift . Set.fromList . map fst . filter ((=='@') . snd) . labelGrid . lines
