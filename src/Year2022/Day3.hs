{-# LANGUAGE CPP #-}

module Year2022.Day3 where

import Data.Char
import Data.List
import Data.List.Split
import qualified Control.Lens as Lens
import Control.Lens.Operators hiding ((??))
import System.FilePath

part1, part2 :: [[Char]] -> [[[Char]]]
part1 = map ((^.. Lens.each) . (splitAt =<< flip div 2 . length))
part2 = chunksOf 3

badge :: [[Char]] -> Int
badge = ((+) <$> (flip mod 32 . ord) <*> ((* 26) . fromEnum . isUpper))
  . head . foldr1 intersect . map nub

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . sum . map badge . part2 . lines

