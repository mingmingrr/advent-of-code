{-# LANGUAGE BangPatterns #-}

import Data.Char

-- h(x, y) = f(x + y - 1) - f(y) + g(y)
-- g(y) = f(y - 1) + 1
-- f(x) = x * (x + 1) / 2
index x y = ((x + y)^2 - x - 3*y + 2) `div` 2

iteration !0 !v = v
iteration !n !v = iteration (n - 1) ((252533 * v) `mod` 33554393)

main = do
  [row, col] <- map read . filter (all isDigit) . words <$> getLine
  print $ iteration (index col row) (20151125 :: Integer)
