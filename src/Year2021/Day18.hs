{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Year2021.Day18 where

import Util hiding (between)
import Text.Megaparsec hiding (parseError)
import Control.Monad
import System.FilePath

data Snailfish
  = Single Int
  | Deep Snailfish Snailfish
  deriving Eq

instance Show Snailfish where
  show (Single x) = show x
  show (Deep x y) = show [x, y]

parser :: ParserSimple Snailfish
parser = between "[" "]" (Deep <$> parser <*> ("," *> parser))
     <|> Single <$> pInteger

explode :: Snailfish -> Maybe Snailfish
explode x = fmap (\(_, x, _) -> x) (explode' 0 x) where
  explode' :: Int -> Snailfish -> Maybe (Int, Snailfish, Int)
  explode' 4 (Deep (Single x) (Single y)) = Just (x, Single 0, y)
  explode' n (Deep (explode' (n + 1) -> Just (a, b, c)) y) =
    Just (a, Deep b (mergeLeft c y), 0) where
      mergeLeft n (Single x) = Single (n + x)
      mergeLeft n (Deep x y) = Deep (mergeLeft n x) y
  explode' n (Deep x (explode' (n + 1) -> Just (a, b, c))) =
    Just (0, Deep (mergeRight a x) b, c) where
      mergeRight n (Single x) = Single (n + x)
      mergeRight n (Deep x y) = Deep x (mergeRight n y)
  explode' _ _ = Nothing

split :: Snailfish -> Maybe Snailfish
split (Single x) | x > 9 =
  Just (Deep (Single (div x 2)) (Single (div (x + 1) 2)))
split (Deep x@(split -> Just x') y) = Just (Deep x' y)
split (Deep x y@(split -> Just y')) = Just (Deep x y')
split _ = Nothing

reduce :: Snailfish -> Snailfish
reduce x@(explode -> Just x') = reduce x'
reduce x@(split -> Just x') = reduce x'
reduce x = x

score :: Snailfish -> Int
score (Single x) = x
score (Deep x y) = 3 * score x + 2 * score y

part1, part2 :: [Snailfish] -> Int
part1 = score . foldl1 (\x y -> reduce (Deep x y))
part2 = maximum . map (score . reduce . uncurry' Deep)
  . filter (uncurry' (/=)) . replicateM 2

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part2 . map (parseError parser) . lines

