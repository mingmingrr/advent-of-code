{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day19 where

import Util
import Debug.Trace
import Linear.V3
import Data.Bifunctor
import Data.List
import Data.List.Extra (replace, dropEnd)
import Data.List.Split
import Control.Lens
import Control.Monad
import System.FilePath
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

type Scanner = ([(Int, V3 Int)], Set (V3 Int))

orientations :: [V3 Int -> V3 Int]
orientations = [ (^. o) . n | o <- [id, _xzy, _yxz, _yzx, _zxy, _zyx],
  n <- [id,negate] ++ map (%~ negate) [_x,_y,_z] ++ map (%~ negate) [_xy,_xz,_yz] ]

merge :: Scanner -> Scanner -> [Scanner]
merge (xn, xs) (yn, ys) = [ (sort (xn ++ map (second transform) yn), zs)
  | orient <- orientations
  , offset <- Map.keys . Map.filter (>= 12) $ counter
      [ x - orient y | x <- Set.toList xs , y <- Set.toList ys ]
  , let transform = (+ offset) . orient
  , let zs = Set.union xs (Set.map transform ys) ]

mergeAll :: [Scanner] -> Scanner
mergeAll xs | trace (unwords . map (("\x1b[48;5;238m"++) . (++"\x1b[0m")) . splitOn "],[" .
  dropEnd 2 . drop 2 . show $ map (map fst . fst) xs) False = undefined
mergeAll [x] = x
mergeAll (x:xs) = mergeAll . sortOn (Set.size . snd) . head $ concat
  [ (:(ts++ds)) <$> merge y x | n <- [0..length xs - 1], let (ts,y:ds) = splitAt n xs ]

part1, part2 :: Scanner -> Int
part1 = Set.size . snd
part2 = maximum . map (sum . abs) . join (liftM2 (-)) . map snd . fst

main = readFile (replaceExtension __FILE__ ".in") >>= \input ->
  print . part2 $ mergeAll
    [ ([(number, 0)], Set.fromList (map (read . ("V3 "++) . replace "," " ") coords))
    | ((readOnlyNums -> [number]):coords) <- paragraphs input ]

