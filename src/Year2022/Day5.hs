{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Year2022.Day5 where

import Util
import Data.List
import Data.List.Split
import Control.Lens
import System.FilePath

part1, part2 :: [a] -> [a]
part1 = reverse
part2 = id

main = readFile (replaceExtension __FILE__ ".in") >>= \(paragraphs -> [boxes, moves]) ->
  putStrLn . concatMap (take 1) . flip (foldl move) (map readOnlyNums moves)
    . ([]:) . map (dropWhile (== ' ') . init) . transpose
    $ map (map head . chunksOf 4 . tail) boxes
  where move xs [n, a, b] = over (ix b) (part2 ts ++) . set (ix a) ds $ xs
          where (ts, ds) = splitAt n (xs !! a)

