{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Year2020.Day9 where

import Data.Foldable
import Data.Semigroup
import Data.List

import Control.Arrow
import qualified Control.Lens as Lens
import Control.Lens.Operators

import System.FilePath

import Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq

takeTilFail :: [Int] -> [Int]
takeTilFail = uncurry look . first Seq.fromList . splitAt 25 where
  look xs@(_:<|xt) (y:ys) =
    if or [a + b == y | a <- toList xs, b <- toList xs]
      then y : look (xt:|>y) ys else [y]

part1, part2 :: [Int] -> Int
part1 = last
part2 (Lens.unsnoc -> Just (xs, total)) = head
  [ getMin (m ^. _1) + getMax (m ^. _2)
  | ys <- tails (map (\x -> (Min x, Max x, Sum x)) xs)
  , m <- scanl (<>) mempty ys
  , getSum (m ^. _3) == total ]

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . part2 . takeTilFail . map read $ lines input

