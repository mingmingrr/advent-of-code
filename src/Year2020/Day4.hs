{-# LANGUAGE CPP #-}

module Year2020.Day4 where

import Year2020.Util

import Text.Regex.PCRE
import Text.Read (readMaybe)

import Data.Char
import Data.List.Split

import System.FilePath

import qualified Data.Set as Set
import Data.Map.Syntax
import Data.Map (Map)
import qualified Data.Map as Map

betweens :: Int -> Int -> String -> Bool
betweens x y s = maybe False (between x y) (readMaybe s)

fields :: Map String (String -> Bool)
fields = runMap' $ do
  "byr" ## betweens 1920 2002
  "iyr" ## betweens 2010 2020
  "eyr" ## betweens 2020 2030
  "ecl" ## (=~ "^(amb|blu|brn|gry|grn|hzl|oth)$")
  "pid" ## (=~ "^[0-9]{9}$")
  "hcl" ## (=~ "^#[0-9a-f]{6}$")
  "hgt" ## \xs -> case span isDigit xs of
     (n, "in") -> betweens 59 76 n
     (n, "cm") -> betweens 150 193 n
     _ -> False

part1, part2 :: Map String String -> Bool
part1 s = Map.keysSet fields `Set.isSubsetOf` Map.keysSet s
part2 s = Map.keysSet fields `Set.isSubsetOf` Map.keysSet s
   && and [f v | (c, f) <- Map.toList fields, let v = s Map.! c]

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . length . filter part2
    . map (Map.fromList . map (tuplify . splitOn ":") . (>>= words))
    $ paragraphs input

