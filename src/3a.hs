{-# LANGUAGE TupleSections, FlexibleContexts, RecordWildCards #-}

import Text.Parsec
import qualified Data.Map as Map
import Data.Either

number :: Stream s m Char => ParsecT s u m Int
number = read <$> many1 digit

data Thing = Thing
  { idnum :: Int
  , l :: Int
  , t :: Int
  , w :: Int
  , h :: Int
  } deriving (Show)

thing = do
  char '#'
  id <- number
  string " @ "
  l <- number
  char ','
  t <- number
  string ": "
  w <- number
  char 'x'
  h <- number
  return $ Thing { idnum = id, w = w, h = h, t = t, l = l }

counter :: (Eq a, Ord a) => [a] -> Map.Map a Int
counter = Map.fromListWith (+) . map (, 1)

tiles Thing{..} = (,) <$> [l..l+w-1] <*> [t..t+h-1]

main = rights . map (parse thing "") . lines <$> getContents
   >>= print . length . filter (>= 2) . Map.elems . counter . concatMap tiles
