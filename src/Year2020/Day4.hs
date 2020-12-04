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

module Year2020.Day4 where

import Year2020.Util
import Year2020.Data.Cyclic (Cyclic, Cyclic2)
import qualified Year2020.Data.Cyclic as Cyclic

import Numeric

import qualified Linear as Lin
import Linear.V2
import Linear.V3
import qualified Linear.Matrix as Mat

import Debug.Trace
import Debug.Pretty.Simple

import Text.Regex.PCRE
import Text.Pretty.Simple
import Text.Read (readMaybe)
import qualified Text.Megaparsec as Par
import qualified Text.Megaparsec.Char as Par

import qualified Data.SBV as SBV
import Data.String.Here
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
import Data.Ord
import Data.Function
import Data.Function.Memoize
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
import qualified Control.Monad.Combinators as Comb
import qualified Control.Monad.Combinators.Expr as Comb

import System.FilePath

import Algebra.Graph (Graph)
import qualified Algebra.Graph as Graph

import Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as PList
import qualified Data.List.PointedList.Circular as PCList
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Syntax
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.Sequence as MinPQ
import Data.PQueue.Prio.Max (MaxPQueue)
import qualified Data.Sequence as MaxPQ

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

betweens :: Int -> Int -> String -> Bool
betweens x y s = maybe False (between x y) (readMaybe s)

fields :: Map String (String -> Bool)
fields = runMap' $ do
  "byr" ## betweens 1920 2002
  "iyr" ## betweens 2010 2020
  "eyr" ## betweens 2020 2030
  "ecl" ## (`elem` words "amb blu brn gry grn hzl oth")
  "pid" ## \xs -> all isDigit xs && length xs == 9
  "hcl" ## (=~ "^#[0-9a-f]{6}$")
  "hgt" ## \xs -> case span isDigit xs of
     (n, "in") -> betweens 59 76 n
     (n, "cm") -> betweens 150 193 n
     _ -> False

part1, part2 :: Map String String -> Bool
part1 s = Map.keysSet fields `Set.isSubsetOf` Map.keysSet s
part2 s = Map.keysSet fields `Set.isSubsetOf` Map.keysSet s
   && and [f v | (c, f) <- Map.toList fields, let v = s Map.! c]

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . length . filter part2
    . map (Map.fromList . map (tuplify . splitOn ":") . (>>= words))
    $ paragraphs input

