{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Year2022.Day7 where

import Util
import Data.Char
import Data.List
import Data.Foldable
import System.FilePath
import qualified Data.Graph.Inductive as Graph
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap

part1 :: [Int] -> Int
part1 = sum . filter (<= 100000)
part2 = minimum . (filter =<< (<=) . subtract 40000000 . maximum)

parse :: [String] -> [[String]] -> [([String], Int)]
parse _ [] = []
parse cwd (["$", "cd", ".."] : cmds) = parse (tail cwd) cmds
parse cwd (["$", "cd", dir] : cmds) = parse (dir:cwd) cmds
parse cwd ([size@((isDigit -> True) : _), dir] : cmds) =
  (dir : cwd, read size) : parse cwd cmds
parse cwd (cmd : cmds) = parse cwd cmds

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  let files@(Map.fromList -> sizes) = parse [] . map words $ lines input
  let (nodes, graph) = fromEdges @Graph.Gr [ (ds, d:ds, 0)
        | (dirs, size) <- files, d:ds <- tails dirs ]
  print $ part2
    [ sum [ sizes !?? (nodes Bimap.! n)
      | n <- toList =<< Graph.dff [node] graph ]
    | (node, file) <- Bimap.toList nodes
    , file `Map.notMember` sizes ]

