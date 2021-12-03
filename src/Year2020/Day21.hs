{-# LANGUAGE CPP #-}

module Year2020.Day21 where

import Util

import Data.List
import Data.List.Split

import System.FilePath

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

parse :: String -> ([String], [String])
parse xs = (words ys, tail $ splitOneOf' ", )" zs) where
  [ys, zs] = splitOn "(" xs

allergens :: [([String], [String])] -> Map String (Set String)
allergens xs = Map.fromList [(z, check z) | z <- nub (concat zs) ] where
  check n = foldr1 Set.intersection [Set.fromList a | (a,b) <- xs, n `elem` b]
  (ys, zs) = unzip xs

part1, part2 :: [([String], [String])] -> String
part1 xs = show . length . filter (`Set.notMember` candidates) $ fst =<< xs
  where candidates = Set.unions . Map.elems $ allergens xs
part2 = intercalate "," . head . filter ((==) =<< nub)
  . mapM Set.toList . Map.elems . allergens

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  putStrLn . part2 . map parse $ lines input

