import Data.Ix
import Data.List

reader [n, _, _, s, _, _, a, _, _, _, _, _, _, b, _] =  (n, read s, read a, read b :: Int)

distance t (n, s, a, b) = let c = a + b in a * s * div t c + s * min a (t `mod` c)

scores deers t = map (index (False, True) . (==m)) d
  where d = map (distance t) deers
        m = maximum d

main = interact $ show . maximum . map sum . transpose
                . flip map [1..2503] . scores . map (reader . words) . lines
