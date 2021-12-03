{-# LANGUAGE CPP #-}

module Year2020.Day1 where

import Util

import Text.Pretty.Simple

import Control.Arrow
import Control.Monad

import System.FilePath

part1, part2 :: [a] -> [[a]]
part1 = replicateM 2
part2 = replicateM 3

main = readFile (replaceExtension __FILE__ ".in")
   >>= mapM_ (pPrint . (id &&& product))
     . filter ((== 2020) . sum)
     . part2
     . map readInteger
     . lines

