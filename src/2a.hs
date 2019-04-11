{-# LANGUAGE TupleSections #-}

import Data.List
import qualified Data.Map as Map

counter :: (Eq a, Ord a) => [a] -> Map.Map a Int
counter = Map.fromListWith (+) . map (, 1)

hash = nub . filter (\x -> x == 2 || x == 3) . Map.elems . counter

main = do
  stuff <- lines <$> readFile "2.in"
  print . product . Map.elems . counter $ (stuff >>= hash)

