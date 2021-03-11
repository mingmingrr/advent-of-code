{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Year2020.Day14 where

import Data.Bits
import Data.Bool
import Data.List.Split

import qualified Control.Lens as Lens
import qualified Numeric.Lens as Lens
import Control.Lens.Operators

import System.FilePath

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

part1, part2 :: (String, IntMap Int) -> [String] -> (String, IntMap Int)
part1 (mask, mem) [x] = (x, mem)
part1 (mask, mem) [x, y] = (mask, IntMap.insert (read x) value mem) where
  pick c = map (bool '0' '1' . (== c)) mask ^. Lens.singular Lens.binary
  value = (pick 'X' .&. read y) .|. pick '1'
part2 (mask, mem) [x] = (x, mem)
part2 (mask, mem) [x, y] = (mask, new <> mem) where
  new = IntMap.fromList [(a ^. Lens.singular Lens.binary, read y)
    | a <- sequence . float mask . pad $ Lens.review Lens.binary (read x)]
  float = zipWith (\case '0' -> pure; '1' -> const "1"; 'X' -> const "01")
  pad xs = replicate (36 - length xs) '0' ++ xs

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . sum . IntMap.elems . snd . foldl part1 (replicate 36 '0', mempty)
    . map (filter (not . null) . splitOneOf " []maskmem=") $ lines input

