{-# LANGUAGE CPP #-}

module Year2022.Day19 where

import Util
import Data.Ord
import Data.List
import Control.Lens
import System.FilePath
import Data.Map (Map)
import qualified Data.Map as Map

type Robots = (Int, Int, Int, Int)
type State = (Robots, Robots)

bfs :: Int -> [Int] -> [State]
bfs n [_, coo, cco, cbo, cbc, cgo, cgb] = foldr go [((1,0,0,0),(0,0,0,0))] [1..n] where
  go n ores = collate n $ concat [ (r, i') :
    [ (_4 +~ 1 $ r, _1 -~ cgo $ _3 -~ cgb $ i') | io >= cgo, ib >= cgb ] ++
    [ (_3 +~ 1 $ r, _1 -~ cbo $ _2 -~ cbc $ i') | io >= cbo, ic >= cbc, rb < cgb ] ++
    [ (_2 +~ 1 $ r, _1 -~ cco $ i') | io >= cco, rc < cbc ] ++
    [ (_1 +~ 1 $ r, _1 -~ coo $ i') | io >= coo, ro < maximum [coo, cco, cbo, cgo] ]
    | (r@(ro, rc, rb, rg), i@(io, ic, ib, ig)) <- ores
    , let i' = (io + ro, ic + rc, ib + rb, ig + rg) ]
  collate n xs = [ (k, v) | (k, vs) <- Map.toList collated
    , v <- nubBy cmpGE . map head . group . sortOn Down $ vs [] ]
    where minima = (2 *) . maximum $ map (\(r, i) -> (i ^. _4) + (r ^. _4) * n) xs
          collated = Map.fromListWith (.) [ (r, (i:)) | (r, i) <- xs
            , 2 * (i ^. _4) + (n * (n + 2 * (r ^. _4) + 1)) >= minima ]
  cmpGE (a, b, c, d) (e, f, g, h) = a >= e && b >= f && c >= g && d >= h

part1, part2 :: (Int, [Int] -> Int)
part1 = (24, sum . zipWith (*) [1..])
part2 = (32, product . take 3)

main = readFile (replaceExtension __FILE__ ".in") >>= print . score
  . map (maximum . map (^. _2 . _4) . bfs limit . readOnlyNums) . lines
  where (limit, score) = part2
