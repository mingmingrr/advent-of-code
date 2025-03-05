{-# LANGUAGE CPP #-}

module Year2024.Day1 where

import Util
import Safe
import Data.List
import System.FilePath

main = readFile (replaceExtension __FILE__ ".in") >>= print
  . sum . uncurry' (zipWith ((abs .) . (-)))
  . map sort . transpose . map readOnlyNums . lines

