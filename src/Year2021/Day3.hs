{-# LANGUAGE CPP #-}

module Year2021.Day3 where

import Util
import Data.Bits
import Data.Char
import Data.List
import Control.Lens hiding ((??))
import System.FilePath
import qualified Data.Map as Map

part1, part2 :: Int -> [[Int]] -> [Int]
part1 bit = map (xor bit . fst . head . common) . transpose
part2 _ [n] = n
part2 bit nums = next : part2 bit (nums ^.. lens) where
  next = bit `xor` (fst . head . common $ map (xor 1 . head) nums)
  lens = folded . filtered ((== next) . head) . _tail

main = readFile (replaceExtension __FILE__ ".in")
  >>= print . ((*) <$> readBase 2 . f 1 <*> readBase 2 . f 0)
    . map (map digitToInt) . lines where f = part2

