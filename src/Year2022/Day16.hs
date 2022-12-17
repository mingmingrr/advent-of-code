{-# LANGUAGE CPP #-}

module Year2022.Day16 where

import Util
import Data.Bits
import Control.Lens
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath

type Mask = Int
type Valve = ([String], Int, Mask, [(Int, String, Mask)])
type Valves = Map String Valve

parse :: Valves -> Int -> [String] -> (String, Valve)
parse valves mask (_:x:_:_:n:_:_:_:_:xs) =
  (x, (map (filter (/= ',')) xs, head (readOnlyNums n), shiftL 1 mask, paths))
  where paths = [ (time + 1, posn, v ^. _3)
          | (time, posn:_) <- tail $ dijkstra neigh x
          , let v = valves Map.! posn , (v ^. _2) /= 0 ]
        neigh p = map ((,) 1) $ (valves Map.! p) ^. _1

part1, part2 :: (Int, Int, [(Mask, Int)] -> Bool)
part1 = (30, 1, const True)
part2 = (26, 2, \[(x, _), (y, _)] -> x .&. y == 0)

subseqs :: Int -> Valves -> [(Mask, Int)]
subseqs limit valves = run 0 0 "AA" where
  run time seen posn = case compare time limit of
    LT -> (seen, flow) : [ (seen', flow + flow')
      | (time', posn', mask) <- neighs , seen .&. mask == 0
      , (seen', flow') <- run (time + time') (seen .|. mask) posn' ]
    _ -> []
    where (_, rate, _, neighs) = valves Map.! posn
          flow = (limit - time) * rate

main = readFile (replaceExtension __FILE__ ".in") >>= \input ->
  let valves = Map.fromList . zipWith (parse valves) [0..] . map words $ lines input
      (limit, agents, check) = part2
   in print . maximum . map (sum . map snd)
        . filter check . replicateM agents . Map.toList
        . Map.fromListWith max $ subseqs limit valves
