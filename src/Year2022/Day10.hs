{-# LANGUAGE CPP #-}

module Year2022.Day10 where

import Text.Read
import Data.List.Split
import System.FilePath

part1, part2 :: [Int] -> String
part1 = show . sum . map head . take 6
  . chunksOf 40 . drop 20 . zipWith (*) [0..]
part2 = unlines . take 6 . chunksOf 40
  . map ((".#"!!) . fromEnum . (<= 2) . flip mod 40)
  . zipWith subtract [-1..]

main = readFile (replaceExtension __FILE__ ".in") >>=
  putStrLn . part1 . scanl (+) 1 . map (maybe 0 id . readMaybe) . words

