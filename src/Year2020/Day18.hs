{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day18 where

import Util

import qualified Text.Megaparsec.Char.Lexer as Lex

import Data.Char
import Data.Functor

import Control.Applicative
import qualified Control.Monad.Combinators as Comb
import qualified Control.Monad.Combinators.Expr as Comb

import System.FilePath

expr :: ParserSimple Int
expr = Comb.makeExprParser (Lex.decimal <|> Comb.between "(" ")" expr) $
  part2 [Comb.InfixL (parserSimple "+" $> (+)), Comb.InfixL ("*" $> (*))]

part1, part2 :: [Comb.Operator ParserSimple Int] -> [[Comb.Operator ParserSimple Int]]
part1 = pure
part2 = map pure

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . sum . map (parseError expr . filter (not . isSpace)) $ lines input

