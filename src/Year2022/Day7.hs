{-# LANGUAGE CPP #-}

module Year2022.Day7 where

import System.FilePath

part1, part2 :: [Int] -> Int
part1 = sum . filter (<= 100000)
part2 = minimum . (filter =<< (<=) . subtract 40000000 . maximum)

parse :: [Int] -> [[String]] -> [Int]
parse stack [] = scanl (+) 0 stack
parse (size:top:stack) (["$", "cd", ".."] : cmds) =
  size : parse (size + top : stack) cmds
parse stack (["$", "cd", _] : cmds) = parse (0:stack) cmds
parse (top:stack) ([size@(n : _), _] : cmds) | n `elem` ['0'..'9'] =
  parse (read size + top : stack) cmds
parse stack (_ : cmds) = parse stack cmds

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part2 . parse [0] . map words . lines

