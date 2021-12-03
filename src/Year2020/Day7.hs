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

module Year2020.Day7 where

import Util

import qualified Text.Megaparsec as Par
import qualified Text.Megaparsec.Char as Par

import Data.Functor

import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Applicative
import qualified Control.Monad.Combinators as Comb

import System.FilePath

import qualified Data.Graph.Inductive as Graph
import qualified Data.Bimap as Bimap
import qualified Data.Tree as Tree

parser :: ParserSimple [(String, String, Int)]
parser = fmap concat . many $ do
  name <- Comb.someTill Par.anySingle (Par.string " bags contain ")
  let subbag = do
        count <- pInteger <* Par.space
        name' <- Comb.someTill Par.anySingle (Par.string " bag")
        optional (Par.single 's')
        return (name, name', count)
  subs <- Par.string "no other bags" $> []
      <|> Comb.sepBy1 subbag (Par.string ", ")
  Par.single '.'
  Par.eof <|> void Par.eol
  return subs

part1, part2 :: Graph.Node -> Gr String Int -> Int
part1 start graph = pred . length . Graph.reachable start $ Graph.grev graph
part2 start graph = pred . Tree.foldTree (\x ys -> 1 + sum (zipWith (*) x ys))
  . head $ xdffWith' Graph.suc' (^.. Lens._4 . Lens.folded . Lens._1) [start] graph

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  let (bimap, graph) = fromEdges $ parseError parser input
      start = bimap Bimap.!> "shiny gold"
  print $ part2 start graph

