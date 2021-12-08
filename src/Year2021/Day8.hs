{-# LANGUAGE CPP #-}

module Year2021.Day8 where

import Util
import Data.Maybe
import Data.List
import Data.List.Split
import System.FilePath

table :: [String]
table = words "abcdef bc abdeg abcdg bcfg acdfg acdefg abc abcdefg abcdfg"

part1, part2 :: [String] -> [String] -> Int
part1 = const (length . filter ((<= 4) . length))
part2 xs ys = head $ do
  ordering <- permutations "abcdefg"
  let findChar x = fromJust (lookup x (zip "abcdefg" ordering))
      from7seg = flip elemIndex table . sort . map findChar
  maybeToList $ read . concatMap show
    <$ traverse from7seg xs <*> traverse from7seg ys

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . sum . map (uncurry' part2 . map words . splitOn " | ") . lines

