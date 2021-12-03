{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Year2020.Day12 where

import Util

import Linear.V2

import Data.List

import Control.Arrow
import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Monad.State

import System.FilePath

import qualified Data.Map as Map

data Ship = Ship
  { _pos :: V2 Int
  , _dir :: V2 Int
  } deriving Show
Lens.makeLenses ''Ship

part1, part2 :: (V2 Int, Lens.ReifiedLens' Ship (V2 Int))
part1 = (V2 1 0, Lens.Lens pos)
part2 = (V2 10 0, Lens.Lens dir)

move :: Lens.Lens' Ship (V2 Int) -> String -> State Ship ()
move lens (fmap (second read) . uncons -> Just (chr, step)) =
  case directions Map.!? chr of
    Just d -> lens += fromIntegral step * d
    Nothing -> case chr of
      'F' -> Lens.uses dir (* fromIntegral step) >>= (pos +=)
      'R' -> dir %= (!! mod (div step 90) 4) . iterate clockWise
      'L' -> dir %= (!! mod (div step 90) 4) . iterate counterClockWise

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  let (ship, lens) = first (Ship (V2 0 0)) part2
  print . sum . abs . _pos . flip execState ship
    . mapM_ (move (Lens.runLens lens)) $ lines input

