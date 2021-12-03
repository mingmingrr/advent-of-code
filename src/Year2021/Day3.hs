{-# LANGUAGE CPP #-}

module Year2021.Day3 where

import Util
import Data.Bits
import Data.Char
import Data.List
import Control.Lens
import Control.Lens.Operators hiding ((??))
import System.FilePath
import qualified Data.Map as Map

popular :: [Int] -> Int
popular nums = fromEnum (counts 1 >= counts 0)
  where counts n = counter nums Map.!? n ?? 0

binary :: Integral a => [a] -> Integer
binary = foldl (\n x -> n * 2 + toInteger x) 0

part1, part2 :: Int -> [[Int]] -> [Int]
part1 bit = map (xor bit . popular) . transpose
part2 _ [n] = n
part2 bit nums = next : part2 bit (nums ^.. lens) where
  next = bit `xor` popular (map head nums)
  lens = folded . filtered ((== next) . head) . _tail

main = readFile (replaceExtension __FILE__ ".in")
  >>= print . ((*) <$> binary . f 1 <*> binary . f 0)
    . map (map digitToInt) . lines where f = part1

