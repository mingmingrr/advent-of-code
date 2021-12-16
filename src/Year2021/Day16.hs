{-# LANGUAGE CPP #-}

module Year2021.Day16 where

import Util
import Numeric.Char
import Text.Printf
import Text.Megaparsec
import Data.Char
import Control.Monad.Loops
import System.FilePath
import Data.Tree (Tree(..))
import Data.Generics

type Ast = Tree (Int, Either Int Int) -- Either Literal Operator

pNumber :: Int -> ParserSimple Int
pNumber n = readBase 2 <$> count n anySingle

parser :: ParserSimple Ast
parser = try literal <|> try opCount <|> try opLength

literal :: ParserSimple Ast
literal = do
  version <- pNumber 3
  chunk [1,0,0]
  left <- many (single 1 *> pNumber 4)
  right <- single 0 *> pNumber 4
  return (Node (version, Left (readBase 16 (left ++ [right]))) [])

opLength :: ParserSimple Ast
opLength = do
  version <- pNumber 3
  optype <- pNumber 3
  single 0
  sublength <- pNumber 15
  offset <- getOffset
  children <- parser `untilM` fmap (== sublength + offset) getOffset
  return (Node (version, Right optype) children)

opCount :: ParserSimple Ast
opCount = do
  version <- pNumber 3
  optype <- pNumber 3
  single 1
  subcounts <- pNumber 11
  children <- count subcounts parser
  return (Node (version, Right optype) children)

part1, part2 :: Ast -> Int
part1 = sum . map (fst . rootLabel) . listify (const True :: Ast -> Bool)
part2 (Node (_, Left x) []) = x
part2 (Node (_, Right 0) xs) = sum (map part2 xs)
part2 (Node (_, Right 1) xs) = product (map part2 xs)
part2 (Node (_, Right 2) xs) = minimum (map part2 xs)
part2 (Node (_, Right 3) xs) = maximum (map part2 xs)
part2 (Node (_, Right 5) [x,y]) = if part2 x > part2 y then 1 else 0
part2 (Node (_, Right 6) [x,y]) = if part2 x < part2 y then 1 else 0
part2 (Node (_, Right 7) [x,y]) = if part2 x == part2 y then 1 else 0

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part1 . Util.parseError parser
    . concatMap (printf "%04b" . digitToInt) . head . lines

