reader [n, _, _, s, _, _, a, _, _, _, _, _, _, b, _] =  (n, read s, read a, read b :: Int)

distance t (n, s, a, b) = let c = a + b in a * s * div t c + s * min a (t `mod` c)

main = interact $ show . maximum . map (distance 2503 . reader . words) . lines
