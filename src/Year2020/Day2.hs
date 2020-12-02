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

module Year2020.Day2 where

import Year2020.Util
import Numeric

import qualified Linear as Lin
import Linear.V2
import Linear.V3
import qualified Linear.Matrix as Mat

import Debug.Trace
import Debug.Pretty.Simple

import Text.Pretty.Simple
import Text.Megaparsec as Parsec

import qualified Data.SBV as SBV
import Data.Word
import Data.Bits
import Data.Ratio
import Data.Bool
import Data.Bifunctor
import Data.Complex
import Data.Foldable
import Data.Char
import Data.Monoid
import Data.Functor
import Data.Function
import Data.Either
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Conduit as Cond
import Data.Conduit ((.|))

import Control.Arrow
import Control.Concurrent
import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Except

import System.FilePath.Posix

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

part1, part2 :: [String] -> Bool
part1 [a, b, [c], _, s] = read a <= x && x <= read b
   where x = fromMaybe 0 (counter s Map.!? c)
part2 [a, b, [c], _, s] = xor (s !! (read a - 1) == c) (s !! (read b - 1) == c)

main = readFile (replaceExtension __FILE__ ".in") >>=
   print . length . filter part2 . map (splitOneOf ":- ") . lines
