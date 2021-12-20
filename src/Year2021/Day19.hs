{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day19 where

import Util
import Debug.Trace
import Linear.V3
import Data.Bifunctor
import Data.List.Extra
import Control.Lens
import Control.Monad
import System.FilePath
import qualified Data.Map.Strict as Map

type Scanner = ([(Int, V3 Int)], [V3 Int])

orientations :: [V3 Int -> V3 Int]
orientations = [ (^. o) . n | o <- [id, _xzy, _yxz, _yzx, _zxy, _zyx],
  n <- [id,negate] ++ map (%~ negate) [_x,_y,_z] ++ map (%~ negate) [_xy,_xz,_yz] ]

duplicated :: Ord a => [a] -> [a]
duplicated = run Map.empty where
  run _ [] = []
  run m (x:xs) = case Map.insertLookupWithKey (\_ _ -> (+ 1)) x 1 m of
    (Just 11, m') -> x : run m' xs
    (_, m') -> run m' xs

mergeScanner :: Scanner -> Scanner -> [Scanner]
mergeScanner (xn, xs) (yn, ys) = [ (sort (xn ++ map (second transform) yn), zs)
  | orient <- orientations
  , offset <- duplicated [ x - orient y | x <- xs , y <- ys ]
  , let transform = (+ offset) . orient
  , let zs = nubOrd (xs ++ map transform ys) ]

mergeAll :: [Scanner] -> Scanner
mergeAll xs | trace (unwords . map (("\x1b[48;5;238m"++) . (++"\x1b[0m")) . splitOn "],[" .
  dropEnd 2 . drop 2 . show $ map (map fst . fst) xs) False = undefined
mergeAll [x] = x
mergeAll (x:xs) = mergeAll . sortOn (length . snd) . head $ concat
  [ (:(ts++ds)) <$> mergeScanner y x | (n,y) <- zip [0..] xs, let (ts,_:ds) = splitAt n xs ]

part1, part2 :: Scanner -> Int
part1 = length . snd
part2 = maximum . map (sum . abs) . join (liftM2 (-)) . map snd . fst

main = readFile (replaceExtension __FILE__ ".in") >>= \input ->
  print . part2 $ mergeAll
    [ ([(number, 0)], map (read . ("V3 "++) . replace "," " ") coords)
    | ((readOnlyNums -> [number]):coords) <- paragraphs input ]

