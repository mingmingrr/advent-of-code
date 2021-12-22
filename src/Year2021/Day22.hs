{-# LANGUAGE CPP #-}

module Year2021.Day22 where

import Util
import Linear.V3
import Data.List.Split
import Control.Lens
import System.FilePath

type Cuboid = (V3 Int, V3 Int)

parse :: String -> (Bool, Cuboid)
parse xs = (status == "on", (V3 x1 y1 z1, V3 x2 y2 z2 + 1))
  where status:words = splitOneOf " =.," xs
        [_, x1, _, x2, _, y1, _, y2, _, z1, _, z2] = map read words

(//) :: Cuboid -> Cuboid -> [Cuboid]
(v0, v3) // (v0', v3')
  | any (<= 0) (v2 - v1) = [(v0, v3)]
  | otherwise = [ (x, y)
    | vs <- mapM (zip <*> tail) (sequence [v0, v1, v2, v3])
    , let p@(x, y) = (fst <$> vs, snd <$> vs)
    , p /= (v1, v2) , all (> 0) (y - x) ]
  where v1 = max <$> v0 <*> v0'
        v2 = min <$> v3 <*> v3'

reboot :: [Cuboid] -> (Bool, Cuboid) -> [Cuboid]
reboot xs (True, x) = x : (xs >>= (// x))
reboot xs (False, x) = xs >>= (// x)

part1, part2 :: V3 Int -> Bool
part1 = all (<= 50) . abs
part2 = const True

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . sum . map (product . uncurry subtract) . foldl reboot []
    . filter (allOf both part1 . snd) . map parse . lines

