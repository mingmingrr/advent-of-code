import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map

reader :: [String] -> (String, Map String Int)
reader [a, _, "gain", n, _, _, _, _, _, _, b] = (a, Map.singleton (init b) (read n))
reader [a, _, "lose", n, _, _, _, _, _, _, b] = (a, Map.singleton (init b) (negate $ read n))

happiness people order = sum $ zipWith f order (last order : init order)
  where f a b = (people ! a ! b) + (people ! b ! a)

main = do
  people <- Map.fromListWith (<>) . map (reader . words) . lines <$> getContents
  print . maximum . map (happiness people) . permutations . Map.keys $ people

