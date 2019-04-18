{-# LANGUAGE TupleSections #-}

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.Char

import Data.Maybe
import Data.Either
import Control.Lens
import Control.Monad.State
import Debug.Trace

line = (,) <$> (string "position=" *> vec2)
           <*> (spaces *> string "velocity=" *> vec2)
  where vec2 = toList <$> (char '<' *> number <* char ',')
                      <*> (number <* char '>')
        number = spaces *> (read <$> ((++) <$> negative <*> many1 digit))
        negative = maybeToList <$> optionMaybe (char '-')
        toList a b = [a, b]

toMap = Map.fromListWith (++) . map (_2 %~ (:[]))

runStep m = toMap $ do
  (k, vs) <- Map.toList m
  v <- vs
  return (zipWith (+) k v, v)

display m = do
  guard ((<= 10) . (!!1) $ zipWith (-) maxima minima)
  return . unlines $ map (\y -> map (\x -> inside x y) xs) ys
  where m' = Map.keys m
        minima = foldr1 (zipWith min) m'
        maxima = foldr1 (zipWith max) m'
        [xs, ys] = zipWith enumFromTo minima maxima
        inside x y = if [x, y] `Map.member` m
                       then '#' else ' '

main = do
  points <- either (error . show) toMap
          . parse (many $ line <* spaces) ""
        <$> getContents
  putStrLn . (\(a, b) -> show a ++ "\n" ++ fromJust b)
    . head
    . filter (isJust . (^. _2))
    . map (_2 %~ display)
    . zip [0..] $ iterate runStep points

