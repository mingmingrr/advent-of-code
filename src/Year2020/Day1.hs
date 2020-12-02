{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Year2020.Day1 where

import Year2020.Util
import Numeric

import qualified Linear as Lin
import Linear.V2
import Linear.V3
import Linear.Matrix

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

part1, part2 :: [a] -> [[a]]
part1 = replicateM 2
part2 = replicateM 3

main = readFile (replaceExtension __FILE__ ".in")
   >>= mapM_ (pPrint . (id &&& product))
     . filter ((== 2020) . sum)
     . part2
     . map readInteger
     . lines

