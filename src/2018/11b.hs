{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Function
import Control.Lens
import Control.Monad
import Numeric.LinearAlgebra

grid :: Integer -> Matrix Double
grid n = build (300, 300) (builder `on` (+1) . round)
  where builder x y = let id = x + 10
                       in fromInteger . subtract 5
                        $ (id * y + n) * id `mod` 1000 `div` 100

main = getContents >>= putStrLn
                     . (\(z, (x, y)) -> show (x+1) ++ "," ++ show (y+1) ++ "," ++ show z)
                     . (_2 %~ snd)
                     . maximumBy (compare `on` snd)
                     . zip ([1..300] :: [Int])
                     . map (liftM2 (,) maxElement maxIndex)
                     . zipWith corr2 (map (konst 1 . join (,)) [1..300])
                     . repeat . grid . read
