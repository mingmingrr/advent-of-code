{-# LANGUAGE CPP #-}

module Year2021.Day10 where

import Util
import Safe
import Data.Maybe
import Data.Either
import Data.List
import System.FilePath
import qualified Data.Bimap as Bimap

pairs :: String -> String -> Either Char String
pairs ns [] = Right ns
pairs [] (x:xs) = pairs [x] xs
pairs (n:ns) (x:xs) = case (== n) <$> Bimap.lookupR x brackets of
  Nothing -> pairs (x:n:ns) xs
  Just True -> pairs ns xs
  _ -> Left x

part1, part2 :: [Either Char String] -> Int
part1 = sum . map (`lookupJust` table) . lefts
  where table = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
part2 = fst . head . filter (uncurry (==)) . (zip <*> reverse) . sort . map score . rights
  where table = [('(', 1), ('[', 2), ('{', 3), ('<', 4)]
        score = foldl (\s x -> s * 5 + x) 0 . map (`lookupJust` table)

main = readFile (replaceExtension __FILE__ ".in") >>= 
  print . part2 . map (pairs []) . lines

