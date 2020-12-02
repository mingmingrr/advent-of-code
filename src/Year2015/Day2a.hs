import Text.Parsec
import Data.Either

box = slack . sides <$> ((read <$> many1 digit) `sepBy1` char 'x')
  where sides [x, y, z] = [x * y, x * z, y * z]
        slack as = toInteger 2 * sum as + minimum as

main = getContents
   >>= print
     . either (error . show) sum
     . parse (box `sepEndBy` newline) ""
