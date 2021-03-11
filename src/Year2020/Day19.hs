{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Year2020.Day19 where

import Year2020.Util

import qualified Text.Megaparsec as Par
import qualified Text.Megaparsec.Char as Par
import qualified Text.Megaparsec.Char.Lexer as Lex

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Combinators as Comb

import System.FilePath

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Rule = String -> [String]

rule :: IntMap Rule -> ParserSimple (IntMap Rule)
rule dict = fmap IntMap.fromList . (`Comb.sepEndBy` Par.eol) $ do
  n <- Lex.decimal <* ": "
  let ph = foldl1 (>=>) <$> some ((dict IntMap.!) <$> Lex.decimal <* Par.hspace)
      ch c = \case [] -> [] ; x:xs -> [xs | c == x]
  r <- "\"" *> fmap ch Par.anySingle <* "\""
   <|> (concat .) . sequence <$> Comb.sepBy1 ph "| "
  return (n, r)

matching :: [String] -> [String] -> [String]
matching rules = filter (any null . (dict IntMap.! 0)) where
  dict = parseError (rule dict <* Par.eof) (unlines rules <> part2)

part1, part2 :: String
part1 = ""
part2 = "8: 42 | 42 8\n11: 42 31 | 42 11 31"

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . length . uncurry matching . tuplify $ paragraphs input

