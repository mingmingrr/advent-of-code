{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

module Year2022.Day14 where

import Util
import Linear.V2
import Data.List
import Data.List.Split
import Control.Lens
import Control.Monad
import System.FilePath
import Data.Set (Set)
import qualified Data.Set as Set

part1, part2 :: a -> Maybe a
part1 = const Nothing
part2 = Just

place :: Int -> V2 Int -> Set (V2 Int) -> Maybe (Set (V2 Int))
place limit sand rocks
  | sand `Set.member` rocks = Nothing
  | sand ^. _y == limit = part2 $ Set.insert sand rocks
  | move 1 `Set.notMember` rocks = place limit (move 1) rocks
  | move 0 `Set.notMember` rocks = place limit (move 0) rocks
  | move 2 `Set.notMember` rocks = place limit (move 2) rocks
  | otherwise = Just $ Set.insert sand rocks
  where move n = sand + V2 (n - 1) 1

main = readFile (replaceExtension __FILE__ ".in") >>= \input ->
  let rocks = Set.fromList . concatMap
        ( concatMap (mapM (uncurry' enumFromTo . sort))
        . (zipWith (liftM2 variadicList) <*> tail)
        . map (uncurry' V2) . chunksOf 2 . readOnlyNums ) $ lines input
      limit = maximum (rocks ^.. folded . _y) + 1
   in print . length $ unfoldr (fmap ((),) . place limit (V2 500 0)) rocks

