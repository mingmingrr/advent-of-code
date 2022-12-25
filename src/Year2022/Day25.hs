{-# LANGUAGE CPP #-}

module Year2022.Day25 where

import Safe
import Numeric
import System.FilePath

main = readFile (replaceExtension __FILE__ ".in") >>=
  putStrLn . dropWhile (== '0') . ($ "") . show' . sum . map (fst . head . read') . lines
  where read' = readInt 5 (const True) (subtract 2 . (`elemIndexJust` "=-012"))
        show' = showIntAtBase 5 ("=-012" !!) . (+) (iterate ((+ 2) . (* 5)) 0 !! 100)

