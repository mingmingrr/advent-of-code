{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Year2020.Day22 where

import Year2020.Util

import Data.Bool
import Data.Foldable
import Data.Functor

import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Monad
import Control.Monad.State

import System.FilePath

import Data.Set (Set)
import Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq

type Deck = Seq Int

part1, part2 :: Bool
part1 = False
part2 = True

tryTake :: Int -> Seq a -> Maybe (Seq a)
tryTake n xs = guard (Seq.length xs >= n) $> Seq.take n xs

play :: Deck -> Deck -> State (Set [Int]) (Bool, Deck)
play xs Seq.Empty = pure (True, xs)
play Seq.Empty ys = pure (False, ys)
play xs@(x:<|xt) ys@(y:<|yt) =
  Lens.contains (toList xs ++ [0] ++ toList ys) <<.= True >>= \case
    True -> pure (True, xs)
    False -> uncurry play . bool (xt, yt:|>y:|>x) (xt:|>x:|>y, yt) $
      case (part2, tryTake x xt, tryTake y yt) of
        (True, Just x, Just y) -> fst (evalState (play x y) mempty)
        (_, _, _) -> x > y

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . sum . zipWith (*) [1..] . reverse . toList . snd
    . flip evalState mempty . uncurry play . tuplify
    . map (Seq.fromList . map (read :: String -> Int) . tail)
    $ paragraphs input

