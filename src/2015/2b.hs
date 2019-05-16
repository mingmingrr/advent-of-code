import Text.Parsec
import Data.Either

box = ribbon <$> ((read <$> many1 digit) `sepBy1` char 'x')
  where ribbon as = toInteger 2 * (sum as - maximum as) + product as

main = getContents
   >>= print
     . either (error . show) sum
     . parse (box `sepEndBy` newline) ""
