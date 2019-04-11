{-# LANGUAGE FlexibleContexts #-}

import Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as PList
import qualified Data.List.PointedList.Circular as PList
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.Char

data Game = Game
  { marbles :: PointedList Integer
  , turn :: Integer
  , scores :: Map Int Integer
  } deriving (Show)

start = Game
  { marbles = PList.singleton 0
  , turn = 0
  , scores = Map.empty
  }

parser :: Stream s m Char => ParsecT s u m (Integer, Integer)
parser = do
  players <- read <$> many1 digit
  string " players; last marble is worth "
  maxTurn <- read <$> many1 digit
  string " points"
  spaces
  return (players, maxTurn)

main = do
  Right (players, maxTurn) <- parse parser "" <$> readFile "9.in"
  print start
