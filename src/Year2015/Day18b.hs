{-# LANGUAGE TupleSections #-}

import Linear
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

withCorners = (<>Set.fromList (liftM2 V2 [1,100] [1,100]))

mapToSet = withCorners . Set.fromList . map fst . filter ((=='#') . snd) . concat
         . zipWith (flip zipWith [1..] . ((,) .) . flip V2) [1..] . lines

life s = withCorners . Map.keysSet . Map.filterWithKey alive
       . Map.fromListWith (+) . map (,1)
       . concatMap (flip map (liftM2 V2 [-1..1] [-1..1]) . (+))
       $ Set.toList s
  where inBounds = all $ liftM2 (&&) (>= 1) (<= 100)
        alive k 4 = inBounds k && Set.member k s
        alive k 3 = inBounds k
        alive _ _ = False

main = interact $ show . Set.size .  (!!100) . iterate life . mapToSet 

