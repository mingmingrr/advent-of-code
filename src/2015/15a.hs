import Data.Function
import Data.Either
import Text.Parsec

ingredient :: String -> [Int]
ingredient = (either (error . show) id .) . flip parse "" . fmap (map read) $
  many1 letter *> char ':' *> spaces *> flip sepBy (char ',' *> spaces)
  (many1 letter *> spaces *> ((:) <$> option '0' (char '-') <*> many1 digit))

permute score [] = []
permute score [a] = [map (*score) a]
permute score (a:as) = [0..score] >>= \s ->
  map (zipWith (+) (map (*s) a)) (permute (score - s) as)

main = interact $ show . maximum . map (product . map (max 0) . init)
                . permute 100 . map ingredient . lines
