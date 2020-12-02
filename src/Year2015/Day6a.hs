{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Data.Functor
import Data.Either
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map

falsey False = Nothing
falsey True = Just True

number = read <$> many1 digit

action = try (string "turn on" $> const True)
     <|> try (string "turn off" $> const False)
     <|> (string "toggle" $> not)

squares = do
  x1 <- number <* char ','
  y1 <- number
  spaces *> string "through" *> spaces
  x2 <- number <* char ','
  y2 <- number
  return $ ((,) <$> [x1..x2] <*> [y1..y2] :: [(Int, Int)])

parser = do
  act <- action <* spaces
  sqs <- squares
  return $ map (Map.alter $ falsey . act . fromMaybe False) sqs

main = getContents
   >>= print . Map.size
     . foldl' (flip ($!)) Map.empty
     . concatMap (either (error . show) id . parse parser "")
     . lines

