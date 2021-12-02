{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Year2020.Day11 where

import Year2020.Util
import Year2020.Data.Grid (Grid)
import qualified Year2020.Data.Grid as Grid

import Linear.V2

import System.FilePath

type Seats = Grid Char
type Look = Seats -> [V2 Int] -> V2 Int

check :: Seats -> Char -> V2 Int -> Bool
check m v k = m Grid.!? k == Just v

around :: Look -> Seats -> V2 Int -> Int
around f m k = length [()
  | d <- adjacent <> diagonal
  , check m '#' . f m . tail $ iterate (+ d) k ]

iter :: (Int, Look) -> Seats -> Seats
iter (n, f) m = if m == m' then m' else iter (n, f) m' where
  m' = flip Grid.parMapWithKey m $ \(x, y) -> \case
    '#' -> if around f m (V2 x y) >= n then 'L' else '#'
    'L' -> if around f m (V2 x y) == 0 then '#' else 'L'
    '.' -> '.'

part1, part2 :: (Int, Look)
part1 = (4, const head)
part2 = (5, \m -> head . dropWhile (check m '.'))

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . length . filter (== '#') . concat . Grid.toLists
    . iter part2 . Grid.fromLists $ lines input

