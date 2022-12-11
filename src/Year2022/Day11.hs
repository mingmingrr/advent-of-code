{-# LANGUAGE CPP #-}

module Year2022.Day11 where

import Util
import Data.List
import Data.List.Split
import Data.Ord
import Control.Monad.Writer
import Control.Monad.State
import Control.Lens
import System.FilePath

type Monkey = ([Int], Int -> Int, [Int])

parse :: [String] -> Monkey
parse (index:items:update:check) = (readOnlyNums items, update', check') where
  update' = case reverse (words update) of
    ("old":fn:_) -> maybe undefined join (lookup fn funcs)
    (num:fn:_) -> maybe undefined ($ read num) (lookup fn funcs)
  funcs = [("+", (+)), ("*", (*))]
  check' = _tail . each %~ subtract (head (readOnlyNums index)) $ readOnlyNums =<< check

run :: (Int -> Int) -> StateT [Monkey] (Writer ([Int] -> [Int])) ()
run modulus = do
  indexed <- gets (flip mod . length)
  (items, modworry, check') <- gets head
  let [check, target1, target2] = check'
  tell (length items :)
  forM_ items $ \item -> do
    let item' = modulus (modworry item)
        towards = if mod item' check == 0 then target1 else target2
    ix (indexed towards) . _1 %= (item':)
  modify ((++) <$> tail <*> pure . set _1 [] . head)

part1, part2 :: (Int, Int -> Int -> Int)
part1 = (20, const (`div` 3))
part2 = (10000, flip mod)

main = do
  monkeys <- map parse . paragraphs <$> readFile (replaceExtension __FILE__ ".in")
  let (rounds, modulus) = part2
  print . product . take 2 . sortOn Down . foldl1 (zipWith (+))
    . take rounds . chunksOf (length monkeys) . ($ [])
    . execWriter . flip runStateT monkeys . forever
    . run . modulus . product $ map (^. _3 . to head) monkeys
