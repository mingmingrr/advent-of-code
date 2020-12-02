{-# LANGUAGE TupleSections #-}

import Linear
import qualified Data.Map as Map
import qualified Data.Set as Set

mapToSet = Set.fromList . map fst . filter ((=='#') . snd) . concat
         . zipWith (flip zipWith [1..] . ((,) .) . flip V2) [1..] . lines

life s = Map.keysSet . Map.filterWithKey alive
       . Map.fromListWith (+) . map (,1) . filter inBounds
       . concatMap (flip map (V2 <$> [-1..1] <*> [-1..1]) . (+))
       $ Set.toList s
  where inBounds = all $ (&&) <$> (>= 1) <*> (<= 100)
        alive k 3 = True
        alive k 4 = k `Set.member` s
        alive _ _ = False

main = interact $ show . Set.size .  (!!100) . iterate life . mapToSet

