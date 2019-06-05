import Data.Maybe
import Data.Set (Set)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set

reader [x, "to", y, "=", n] = [(y, [(x, read n)]), (x, [(y, (read n :: Int))])]

dfs g n = run (Set.singleton n) g n
  where run seen graph node
          | null leafs = [([node], 0)]
          | otherwise = do
            (n, v) <- leafs
            (ns, v') <- run (Set.insert n seen) graph n
            return $ (node:ns, v + v')
          where leafs = maybe []
                        (filter (flip Set.notMember seen . fst))
                        (Map.lookup node graph)

main = do
  graph <- Map.fromListWith (++)
         . concatMap (reader . words) . lines
       <$> getContents
  print . maximum . map snd
        . filter ((==Map.size graph) . length . fst)
        $ concatMap (dfs graph) (Map.keys graph)

