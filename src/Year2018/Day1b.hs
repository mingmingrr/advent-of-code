import Data.List
import qualified Data.Set as Set

main = do
  list <- scanl1 (+) . cycle . map (read . filter (/= '+')) . lines <$> getContents
  let sets = scanl (flip Set.insert) Set.empty list
  print . fst . head . filter (uncurry Set.member) $ zip list sets
