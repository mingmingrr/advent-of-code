import Data.List
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map

singleton a b n = (a, Map.singleton b n)

reader :: [String] -> (String, Map String Int)
reader [a, _, "gain", n, _, _, _, _, _, _, b] = singleton a (init b) (read n)
reader [a, _, "lose", n, _, _, _, _, _, _, b] = singleton a (init b) (negate $ read n)

happiness people order = sum $ zipWith f order (last order : init order)
  where f a b = lookup a b + lookup b a
        lookup a b = fromMaybe 0 (Map.lookup a people >>= Map.lookup b)

main = do
  people <- Map.fromListWith (<>) . map (reader . words) . lines <$> getContents
  let people' = Map.insert "you" Map.empty people
  print . maximum . map (happiness people') . permutations . Map.keys $ people'

