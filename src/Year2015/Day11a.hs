import Data.Char
import Data.Either
import Text.Parsec
import Control.Monad

valid = (isRight .) . flip parse "" $ do
  notFollowedBy $ manyTill anyChar (oneOf "iol")
  lookAhead . manyTill anyChar . try $ do
    [a, b, c] <- count 3 anyChar
    guard $ (a == pred b) && (b == pred c)
  lookAhead . manyTill anyChar . try $ do
    a <- anyChar >>= char
    manyTill anyChar . try $
      anyChar >>= char >>= guard . (/=a)

next = reverse . iter . reverse
  where iter [] = []
        iter ('z':as) = 'a' : iter as
        iter (a:as) = succ a : as

main = interact $ until valid next . next . head . lines

