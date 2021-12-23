{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day23 where

import Util
import Safe
import Data.Char
import Data.Maybe
import Data.List
import Control.Lens
import System.FilePath

waitSlot, roomSlot :: [Int]
waitSlot = [1,2,4,6,8,10,11]
roomSlot = [3,5,7,9]

part1, part2 :: Int
part1 = 2
part2 = 4

neighbor :: Int -> ([Maybe Int], [[Int]]) -> [(Int, ([Maybe Int], [[Int]]))]
neighbor size (waits, rooms) =
  [ (dist * 10^x, (ix i' ?~ x $ waits, ix i .~ rs $ rooms))
  | (i, n, x:rs) <- zip3 [0..] roomSlot rooms
  , let (xs, ys) = span ((< n) . (^. _2)) (zip3 [0..] waitSlot waits)
  , (i', n', _) <- takeWhile (isNothing . (^. _3)) ys ++
    takeWhile (isNothing . (^. _3)) (reverse xs)
  , let dist = abs (n - n') + (size - length rs)
  ] ++
  [ (dist * 10^x, (ix i .~ Nothing $ waits, ix x %~ (x:) $ rooms))
  | (i, n, Just x) <- zip3 [0..] waitSlot waits
  , all (== x) (rooms !! x)
  , let n' = roomSlot !! x
  , and [isNothing x | (t, x) <- zip waitSlot waits, min n n' < t, t < max n n']
  , let dist = abs (n - n') + (size - length (rooms !! x))
  ]

main = readFile (replaceExtension __FILE__ ".in") >>=
  \(map (filter isLetter) . lines -> _:_:line1:line2:_) ->
    print . fst . findJust (((== map (replicate size) [0..3]) . snd) . head . snd)
      $ dijkstra (neighbor size) (replicate 7 Nothing,
        transpose . map (map (subtract 10 . digitToInt)) . take size
          $ drop size ["", "", line1, line2, line1, "DCBA", "DBAC", line2])
  where size = part2

