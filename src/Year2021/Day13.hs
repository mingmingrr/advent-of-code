{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day13 where

import Util
import Linear.V2
import Control.Lens
import System.FilePath
import Data.Set (Set)
import qualified Data.Set as Set

folder :: String -> Set (V2 Int) -> Set (V2 Int)
folder (x:_:xs) = Set.map (l %~ \x -> n - abs (n - x))
  where l = if x == 'x' then _x else _y ; n = read xs

part1, part2 :: [Set (V2 Int)] -> String
part1 = show . Set.size . (!! 1)
part2 = displayGrid . map ((,'#') . (^. _yx)) . Set.toList . last

main = readFile (replaceExtension __FILE__ ".in") >>= \(paragraphs -> [points, folds]) ->
  putStrLn . part1 $ scanl (flip folder)
    (Set.fromList $ map (uncurry' V2 . readOnlyNums) points)
    (map (drop 11) folds)

