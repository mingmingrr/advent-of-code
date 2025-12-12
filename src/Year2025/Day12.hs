{-# LANGUAGE CPP #-}

module Year2025.Day12 where

import Util
import System.FilePath

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . length . filter check . map readOnlyNums . last . paragraphs
  where check (x:y:ns) = x * y >= 9 * sum ns
