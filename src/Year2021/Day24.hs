{-# LANGUAGE CPP #-}

module Year2021.Day24 where

import Util
import Text.Megaparsec
import Data.List.Split
import System.FilePath

part1, part2 :: Ord a => [a] -> a
part1 = maximum
part2 = minimum

parser :: Parsec () [[Int]] [Int]
parser = fmap concat . many . try $ do
  [1, _, x] <- anySingle
  mid <- parser
  [26, y, _] <- anySingle
  let (a, b) = part2 [ (a, b) | a <- [1..9], b <- [1..9], a + x == b - y ]
  return $ [a] ++ mid ++ [b]

main = readFile (replaceExtension __FILE__ ".in") >>= \input ->
  (>> putStrLn "") . mapM_ (putStr . show) . either (error . show) id $
    parse (parser <* eof) "" [ [ read (last (words (xs !! n)))
      | n <- [4,5,15] ] | xs <- chunksOf 18 (lines input) ]

