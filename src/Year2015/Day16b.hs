import Text.Parsec
import Data.Monoid
import Data.Either
import Data.Map (Map, (!))
import qualified Data.Map as Map

gift = Map.fromList
  [ ("children"    , (==3))
  , ("cats"        , (>7))
  , ("samoyeds"    , (==2))
  , ("pomeranians" , (<3))
  , ("akitas"      , (==0))
  , ("vizslas"     , (==0))
  , ("goldfish"    , (<5))
  , ("trees"       , (>3))
  , ("cars"        , (==2))
  , ("perfumes"    , (==1))
  ]

sue = do
  let name = lexeme $ many1 letter <* char ':'
      number = (read :: String -> Int) <$> many1 digit
      lexeme = (<* spaces)
  lexeme $ string "Sue"
  num <- number
  lexeme $ char ':'
  things <- sepBy1 ((,) <$> name <*> number) (lexeme $ char ',')
  return (num, Map.fromList things)

isSue = getAll . mconcat . Map.elems
      . Map.intersectionWith ((All.) . ($)) gift . snd

main = interact $ show . fst . head . filter isSue
                . map (either (error . show) id . parse sue "") . lines

