import Linear hiding (transpose)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import qualified Data.Set as Set

vectorize '^' = V2 0 (-1)
vectorize '<' = V2 (-1) 0
vectorize 'v' = V2 0 1
vectorize '>' = V2 1 0

main = getContents
   >>= print
     . Set.size
     . Set.fromList
     . concatMap (scanl (+) (V2 0 0))
     . transpose
     . chunksOf 2
     . map vectorize
     . head . lines
