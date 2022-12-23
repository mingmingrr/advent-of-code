{-# LANGUAGE CPP #-}

module Year2022.Day23 where

import Util
import Linear.V2
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.FilePath

lookSet :: [[[V2 Int]]]
lookSet = transpose . take 4 . tails . cycle
  $ map (map (directions Map.!) . words)
    ["N NE NW", "S SW SE", "W NW SW", "E NE SE"]

step :: [[V2 Int]] -> Set (V2 Int) -> Maybe (Set (V2 Int))
step lookss elves = if Map.null moves then Nothing else Just $
  Map.foldlWithKey' (\s k [v] -> Set.insert k (Set.delete v s)) elves moves
  where moves = Map.filter (null . tail) . Map.fromListWith (<>) $ concat
          [ take 1 [ (elf + look, [elf]) | looks@(look:_) <- lookss
                   , all (\l -> Set.notMember (elf + l) elves) looks ]
          | elf <- Set.toList elves
          , any (\l -> Set.member (elf + l) elves) (orthogonal <> diagonal) ]

part1, part2 :: [Maybe (Set (V2 Int))] -> Int
part1 = ((-) <$> product . uncurry (-) . bounds <*> Set.size) . fromJust . (!! 10)
part2 = succ . length . takeWhile isJust

main = readFile (replaceExtension __FILE__ ".in") >>= print
  . part2 . tail . flip (scanl ((. step) . (>>=))) lookSet . Just
  . Set.fromList . map fst . filter ((== '#') . snd) . labelGrid . lines

