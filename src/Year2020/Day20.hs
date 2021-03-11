{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Year2020.Day20 where

import Year2020.Util

import Numeric.LinearAlgebra

import Data.Char
import Data.Function
import Data.List hiding (find)
import Data.List.Extra hiding (find)

import Control.Arrow
import Control.Monad

import System.FilePath

import Data.Map (Map)
import qualified Data.Map as Map

type Tile = Matrix Z

makeTile :: [String] -> (Int, Tile)
makeTile (x:xs) = (n, fromLists (map (map (\case '#' -> 1 ; _ -> 0)) xs))
  where n = read (filter isDigit x) :: Int

transforms :: Tile -> [Tile]
transforms m = take 8 . scanl (&) m $ cycle [tr, flipud]

place :: Int -> Map Int Tile -> Map (Int, Int) (Int, Tile)
       -> Int -> Int -> [[[(Int, Tile)]]]
place size tiles placed _ _ | Map.null tiles = pure . transpose
  .  map (map snd) . groupOn (fst . fst) $ Map.toList placed
place size tiles placed x y = do
  let xp = snd (placed Map.! (x - 1, y)) ?? (All, TakeLast 1)
      yp = snd (placed Map.! (x, y - 1)) ?? (TakeLast 1, All)
  (num, tile') <- Map.toList tiles
  tile <- transforms tile'
  guard $ (x == 1 || tile ?? (All, Take 1) == xp)
       && (y == 1 || tile ?? (Take 1, All) == yp)
  let (x', y') = ((+ x) *** (+ 1)) (divMod y size)
  place size (Map.delete num tiles) (Map.insert (x, y) (num, tile) placed) x' y'

part1, part2 :: [[(Int, Tile)]] -> Int
part1 xs = product . map (fst . ($ xs)) $ (.) <$> [head, last] <*> [head, last]
part2 = fromIntegral . minimum . checks . transforms . fromBlocks
  . map (map ((?? (DropLast 1, DropLast 1)) . (?? (Drop 1, Drop 1)) . snd))
  where goal = fromIntegral $ sumElements monster
        check xs = sumElements $ cond (corr2 monster xs) goal 0 goal 0
        checks (x:xs) = map ((sumElements x -) . check) (x:xs)
        monster = snd . makeTile $ "" :
          [ "                  # "
          , "#    ##    ##    ###"
          , " #  #  #  #  #  #   " ]

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  let tiles = map makeTile . filter notNull $ paragraphs input
      size = round . sqrt . fromIntegral $ length tiles
  print . part2 . head $ place size (Map.fromList tiles) mempty 1 1

