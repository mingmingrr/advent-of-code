import Data.Char
import Data.List

toggle x = if isUpper x then toLower x else toUpper x

polymerize n [] = n
polymerize n [a] = a : n
polymerize n (a:b:c)
  | a /= toggle b = polymerize (a:n) (b:c)
  | otherwise = let (t, d) = splitAt 1 n
                 in polymerize d (t ++ c)

main = init <$> readFile "5.in" >>= print . length . polymerize []
