import Data.Char
import Data.Either
import Text.Parsec
import Data.Functor
import Numeric

unquote = (either (error . show) id .) . flip parse ""
        . between (char '"') (char '"') . many
        $ try (string "\\\"" $> '"')
      <|> try (string "\\\\" $> '\\')
      <|> try (string "\\x" *> (toChar <$> count 2 anyChar))
      <|> noneOf "\""

toChar = chr . fst . head . readHex

difference = (-) <$> length <*> length . unquote

main = interact $ show . sum . map difference . lines

