{-# LANGUAGE CPP #-}

module Year2022.Day8 where

import Util
import Data.Grid
import Data.Bool
import Control.Lens
import System.FilePath

scan :: Grid Int -> (Int, Int) -> Int -> (Bool, Int)
scan grid posn height = bimap or product . unzip $
  map (foldr g undefined . tail . flip iterate p . (+)) orthogonal
  where
    p = partsOf each .~ posn ^.. each $ 0
    g = maybe (const (True, 0 :: Int))
      (bool (const (False, 1)) (fmap succ) . (< height)) . (grid !?)

part1, part2 :: [(Bool, Int)] -> Int
part1 = sum . map (fromEnum . fst)
part2 = maximum . map snd

main = readFile (replaceExtension __FILE__ ".in")
  >>= print . part2 . concat . toLists . (parMapWithKey =<< scan)
    . fromLists . map (map (read . pure)) . lines

