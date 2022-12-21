{-# LANGUAGE CPP #-}

module Year2022.Day21 where

import Util
import Safe
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath

data Tree = Var | Tip Double | Op Char Tree Tree

parse :: (Double -> Tree) -> Map String Tree -> [String] -> (String, Tree)
parse f xs ["humn:", y] = ("humn", f (read y))
parse f xs [x, y] = (init x, Tip (read y))
parse f xs [x, a, b, c] = (,) (init x) $ case (xs Map.! a, xs Map.! c) of
  (Tip n, Tip m) -> Tip (lookupJust (head b) operators' n m)
  (a', c') -> Op (head b) a' c'

solve :: Double -> Tree -> Double
solve x Var = x
solve x (Op f (Tip y) z) | f `elem` "-/" = solve (lookupJust f operators' y x) z
solve x (Op f (Tip y) z) = solve (lookupJust (lookupJust f operatorsT) operators' x y) z
solve x (Op f z (Tip y)) = solve (lookupJust (lookupJust f operatorsT) operators' x y) z

part1, part2 :: Double -> Tree
part1 = Tip
part2 = const Var

main = readFile (replaceExtension __FILE__ ".in") >>= \input ->
  let xs = Map.fromList . map (parse part2 xs . words) $ lines input
   in case xs Map.! "root" of
        Tip n -> print $ round n
        Op _ (Tip x) y -> print . round $ solve x y
        Op _ x (Tip y) -> print . round $ solve y x
