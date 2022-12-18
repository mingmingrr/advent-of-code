{-# LANGUAGE CPP #-}

module Year2020.Day24 where

import Util

import Linear.V2

import Data.List.Extra

import System.FilePath

import qualified Data.Set as Set
import qualified Data.Map as Map

parser :: String -> [V2 Int]
parser []           = []
parser ('e':xs)     = V2 1 0    : parser xs
parser ('s':'e':xs) = V2 1 (-1) : parser xs
parser ('s':'w':xs) = V2 0 (-1) : parser xs
parser ('w':xs)     = V2 (-1) 0 : parser xs
parser ('n':'w':xs) = V2 (-1) 1 : parser xs
parser ('n':'e':xs) = V2 0 1    : parser xs

around :: [V2 Int]
around = orthogonal <> [V2 1 (-1), V2 (-1) 1]

life :: [V2 Int] -> [V2 Int]
life xs = filter live . nubOrd $ neighbors (0:around) [] =<< xs where
  xset = Set.fromList xs
  look = length . neighbors around [(`Set.member` xset)]
  live k = case (Set.member k xset, look k) of
    (True, n) -> n `elem` [1, 2]
    (False, n) -> n == 2

part1, part2 :: [V2 Int] -> [V2 Int]
part1 = id
part2 = (!! 100) . iterate life

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . length . part2 . Map.keys . Map.filter odd . counter
    . map (sum . parser) $ lines input

