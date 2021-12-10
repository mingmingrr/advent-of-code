{-# LANGUAGE CPP #-}

module Year2021.Day10 where

import Util
import Data.Maybe
import Data.List
import System.FilePath
import qualified Data.Bimap as Bimap

pairs :: (Char -> Maybe Int, String -> Maybe Int) -> String -> String -> Maybe Int
pairs fs@(f,g) ns [] = g ns
pairs fs@(f,g) [] (x:xs) = pairs fs [x] xs
pairs fs@(f,g) (n:ns) (x:xs) = case (== n) <$> Bimap.lookupR x brackets of
  Nothing -> pairs fs (x:n:ns) xs
  Just True -> pairs fs ns xs
  _ -> f x

part1, part2 :: ((Char -> Maybe Int, String -> Maybe Int), [Int] -> Int)
part1 = ((flip lookup table, const Nothing), sum)
  where table = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
part2 = ((const Nothing, score), median)
  where table = [('(', 1), ('[', 2), ('{', 3), ('<', 4)]
        median = fst . head . filter (uncurry (==)) . (zip <*> reverse) . sort
        score = Just . foldl (\s x -> s * 5 + x) 0 . mapMaybe (`lookup` table)

main = readFile (replaceExtension __FILE__ ".in") >>= 
  print . fold . mapMaybe (pairs fs []) . lines
  where (fs, fold) = part2

