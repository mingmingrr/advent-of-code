import Linear
import qualified Data.Set as Set

vectorize '^' = V2 0 (-1)
vectorize '<' = V2 (-1) 0
vectorize 'v' = V2 0 1
vectorize '>' = V2 1 0

main = getContents
   >>= print
     . Set.size
     . Set.fromList
     . scanl (+) (V2 0 0)
     . map vectorize
     . head . lines
