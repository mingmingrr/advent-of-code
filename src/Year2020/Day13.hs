{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

module Year2020.Day13 where

import Year2020.Util

import Math.NumberTheory.Moduli

import Numeric

import qualified Data.SBV as SBV
import Data.Foldable
import Data.Maybe
import Data.List.Split

import Control.Arrow
import Control.Monad

import System.FilePath

invMod :: Integer -> Integer -> Integer
invMod n m = case invertSomeMod (n `modulo` fromInteger m) of
  Just (SomeMod k) -> getVal k

part1, part2, part2' :: Integer -> [(Integer, Integer)] -> IO Integer
part1 t = return . uncurry (*) . first (subtract t) . minimum
  . map ((\n -> (div (t - 1) n * n + n, n)) . snd)
part2 t = return . snd . foldl func (1,1) where
  func (m,n) (i,v) = (lcm m v, n + m * mod ((-i-n) * invMod m v) v)
part2' t xs = fmap (fromJust . SBV.getModelValue "n")
  . SBV.satWith SBV.yices $ do
    n <- SBV.sInteger "n"
    SBV.constrain $ n SBV..> 0
    forM_ xs $ \(i, x) -> SBV.constrain $
      SBV.sRem (n + fromInteger i) (fromInteger x) SBV..== 0

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  let [time, nums] = lines input
  print <=< part2' (read time) . catMaybes
    . zipWith (\x y -> (x,) <$> readsMaybe readDec y) [0..]
    $ splitOn "," nums

