{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day12 where

import Util
import Data.Char
import Data.List.Split
import Control.Lens hiding ((&))
import System.FilePath
import Data.Bimap hiding (map)
import Data.Graph.Inductive

part1, part2 :: Bool
part1 = True
part2 = False

paths :: Bool -> Gr (Bool, String) a -> Node -> Int
paths double graph posn = case match posn graph of
  (Just (_, _, (_, "end"), _), _) -> 1
  (Just ctx@(_, _, (True, "start"), _), _) -> 0
  (Just ctx@(_, _, (visited, name), out), (set (_3 . _1) True ctx &) -> graph')
    | not (dupe && double) -> sum $ map (paths (dupe || double) graph' . snd) out
    where dupe = isLower (head name) && visited
  _ -> 0

main = readFile (replaceExtension __FILE__ ".in") >>= \xs ->
  let (bimap, gr) = fromEdges . map (uncurry' (,,()) . splitOn "-") $ lines xs
   in print $ paths part2 (undir (nmap (False,) gr)) (bimap !> "start")
