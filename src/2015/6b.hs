{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Data.Functor
import Data.Either
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map

falsey x
  | x <= 0 = Nothing
  | otherwise = Just x

number = read <$> many1 digit

action = try (string "turn on" $> succ)
     <|> try (string "turn off" $> pred)
     <|> (string "toggle" $> (+2))

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
  return $ map (Map.alter $ falsey . act . fromMaybe 0) sqs

main = getContents
   >>= print
     . Map.foldl' (+) 0
     . foldl' (flip ($!)) Map.empty
     . concatMap (either (error . show) id . parse parser "")
     . lines

