{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Year2022.Day11 where

import Util
import Data.List
import Data.Ord
import Control.Monad
import Control.Lens

import System.FilePath

type Monkey = ([Int], Int -> Int, [Int])

parse :: [String] -> Monkey
parse (_:items:update:test) = (readOnlyNums items, update', readOnlyNums =<< test)
  where update' = case reverse (words update) of
          ("old":fn:_) -> maybe undefined join (lookup fn funcs)
          (num:fn:_) -> maybe undefined ($ read num) (lookup fn funcs)
        funcs = [("+", (+)), ("*", (*))]

run :: (Int -> Int) -> Int -> [Monkey] -> [Monkey]
run modulus index monkeys = ix index . _1 .~ [] $ foldr throw monkeys items
  where (items, modworry, [check, target1, target2]) = monkeys !! index
        throw item = ix @[Monkey] towards . _1 %~ (item':)
          where item' = modulus (modworry item)
                towards = if mod item' check == 0 then target1 else target2

runRound :: (Int -> [Monkey] -> [Monkey]) -> [Monkey] -> ([Int], [Monkey])
runRound run monkeys = (thrown, last runs) where
  runs = scanl (flip run) monkeys [0 .. length monkeys - 1]
  thrown = zipWith (\n xs -> (xs !! n) ^. _1 . to length) [0..] (init runs)

part1, part2 :: (Int, Int -> Int -> Int)
part1 = (20, const (`div` 3))
part2 = (10000, flip mod)

main = do
  monkeys <- map parse . paragraphs <$> readFile (replaceExtension __FILE__ ".in")
  let (rounds, modulus) = part1
  print . product . take 2 . sortOn Down . foldl1 (zipWith (+)) . take rounds
    $ unfoldr (Just . runRound (run (modulus (product (map (^. _3 . to head) monkeys))))) monkeys
