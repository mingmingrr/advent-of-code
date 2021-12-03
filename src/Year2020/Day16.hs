{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Year2020.Day16 where

import Util

import qualified Data.SBV as SBV
import Data.Word
import Data.Maybe
import Data.List
import Data.List.Extra
import Data.List.Split

import qualified Control.Lens as Lens
import qualified Numeric.Lens as Lens
import Control.Lens.Operators

import System.FilePath

import qualified Data.Set as Set

part1, part2 :: [Int] -> [([Int],[[Int]])] -> IO Int
part1 departs (_:tickets) = pure . sum $ catMaybes
  [ (t !!) <$> findIndex null s | (t, s) <- tickets ]
part2 departs tickets@((you@(length -> size), _):_) = do
  let collapse = Set.toList . foldr1 Set.intersection . map Set.fromList
      slots = map collapse . transpose . filter (all notNull) $ map snd tickets
  model <- SBV.satWith SBV.z3 $ do
    ns <- SBV.sWord8s $ map (\n -> "n" ++ show n) [1..size]
    let valid n s = SBV.sElem n (map fromIntegral s)
    SBV.constrain $ SBV.sAnd (zipWith valid ns slots)
    SBV.constrain $ SBV.distinct ns
  pure . product . map (you !!) . findIndices (`elem` departs) . flip mapMaybe [1..size] $
    \n -> (fromEnum :: Word8 -> Int) <$> SBV.getModelValue ("n" ++ show n) model

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  let readInts = (^.. Lens.folded . Lens.decimal) . splitOneOf " -,"
      [fields, [_,you], _:others] = map (map readInts) (paragraphs input)
      valid n [a,b,c,d] = a <= n && n <= b || c <= n && n <= d
      possible fields = map (\n -> findIndices (valid n) fields)
      departs = findIndices (isPrefixOf "departure") (head (paragraphs input))
      tickets = map ((,) <*> possible fields) (you:others)
  print =<< part2 departs tickets

