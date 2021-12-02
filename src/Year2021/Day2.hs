{-# LANGUAGE CPP #-}

module Year2021.Day2 where

import System.FilePath

part1, part2 :: Char -> Int -> (Int, Int, Int) -> (Int, Int, Int)
part1 'f' n (horiz, vert, _) = (horiz + n, vert, 0)
part1 'd' n (horiz, vert, _) = (horiz, vert - n, 0)
part1 'u' n (horiz, vert, _) = (horiz, vert + n, 0)
part2 'f' n (horiz, vert, aim) = (horiz + n, vert + aim * n, aim)
part2 'd' n (horiz, vert, aim) = (horiz, vert, aim - n)
part2 'u' n (horiz, vert, aim) = (horiz, vert, aim + n)

main = do
  readFile (replaceExtension __FILE__ ".in") >>= print
    . (\(x, y, z) -> x * abs y)
    . foldl (\xs [c:_, n] -> part2 c (read n) xs) (0, 0, 0)
    . map words . lines

