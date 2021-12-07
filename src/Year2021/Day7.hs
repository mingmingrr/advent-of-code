{-# LANGUAGE CPP #-}

module Year2021.Day7 where

import Util
import Control.Applicative
import System.FilePath

part1, part2 :: Int -> Int
part1 = id
part2 n = div (n * succ n) 2

main = readFile (replaceExtension __FILE__ ".in") >>= 
  print . minimum .  liftA2 map
    (\ns n -> sum (map (part2 . abs . subtract n) ns))
    (enumFromTo <$> minimum <*> maximum)
    . readOnlyNums

