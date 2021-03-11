{-# LANGUAGE CPP #-}

module Year2020.Day23 where

import Data.Char
import Data.List

import Control.Monad
import Control.Monad.ST

import System.FilePath

import qualified Data.Vector.Storable as VecS
import qualified Data.Vector.Storable.Mutable as VecSM

iterations, count :: Int
finalize :: [Int] -> Int
(iterations, count, finalize) = part2 where
  part1 = (100, 9, read . concatMap show . take 8 :: [Int] -> Int)
  part2 = (10000000, 1000000, product . take 2)

runner :: Int -> (Int, [Int]) -> VecS.Vector Int
runner i (n, xs) = runST $ do
  vec <- VecS.unsafeThaw (VecS.fromList xs)
  iter i n vec
  VecS.unsafeFreeze vec

iter :: Int -> Int -> VecSM.MVector s Int -> ST s Int
iter 0 num vec = pure num
iter i num vec = do
  vec' <- VecS.unsafeFreeze vec
  let (_:x:y:z:p:_) = iterate (vec' VecS.!) num
      nexts n = [n - 1, n - 2 .. 0] ++ [count - 1, count - 2 ..]
      next = head [n | n <- nexts num, n `notElem` [x, y, z]]
  n <- VecSM.unsafeRead vec next
  zipWithM_ (VecSM.unsafeWrite vec) [num, next, z] [p, x, n]
  iter (i - 1) p vec

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  let nums@(num:_) = map (pred . read . pure) (filter isDigit input) ++ [9..count-1]
  print . finalize . map succ . tail . flip iterate 0 . (VecS.!) . runner iterations
    $ (num, map snd . sort $ zip nums (tail nums ++ [num]))

