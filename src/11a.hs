{-# LANGUAGE FlexibleContexts #-}

import Data.Function
import Data.Fixed
import Numeric.LinearAlgebra

grid :: Integer -> Matrix Double
grid n = build (300, 300) (builder `on` (+1) . round)
  where builder x y = let id = x + 10
                       in fromInteger . subtract 5
                        $ (id * y + n) * id `mod` 1000 `div` 100

main = getContents >>= putStrLn
                     . (\(x, y) -> show (x+1) ++ "," ++ show (y+1))
                     . maxIndex
                     . corr2 (konst 1 (3, 3))
                     . grid . read
