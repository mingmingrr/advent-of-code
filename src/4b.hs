{-# LANGUAGE TupleSections, FlexibleContexts #-}

import Text.Parsec
import qualified Data.Map as Map
import Data.Either
import Data.Time
import Data.Time.Clock
import Data.List
import Data.Function
import Control.Monad
import Debug.Trace

number :: Stream s m Char => ParsecT s u m Int
number = read <$> many1 digit

time :: Stream s m Char => ParsecT s u m UTCTime
time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M"
   <$> (char '[' *> many (noneOf "]") <* char ']')

data ThingType = Begin { guardId :: Int } | Sleep | Wake deriving (Show, Eq)

data Thing = Thing { tTime :: UTCTime , tType :: ThingType } deriving (Show)

thingy x = try $ Thing <$> time <*> (char ' ' *> x <* newline)

sleepy = Sleep <$ string "falls asleep"
sleepy' = thingy sleepy

wakey = Wake <$ string "wakes up"
wakey' = thingy wakey

beginy = Begin <$> (string "Guard #" *> number <* string " begins shift")
beginy' = thingy beginy

snoozy = do
  s <- minute . tTime <$> sleepy'
  w <- minute . tTime <$> wakey'
  return $ enumFromTo s (w - 1)

guardy = (,)
     <$> (guardId . tType <$> beginy')
     <*> (counter . concat <$> many snoozy)

counter :: (Eq a, Ord a) => [a] -> Map.Map a Int
counter = Map.fromListWith (+) . map (, 1)

minute = (read :: String -> Int) . formatTime defaultTimeLocale "%M"

main = readFile "4.in"
   >>= print
     . snd
     . maximumBy (compare `on` fst)
     . map (\(x, y) ->
         let (minute, count) = maximumBy (compare `on` snd) ((-1, 0) : Map.assocs y)
          in (count, x * minute)
       )
     . Map.assocs
     . Map.fromListWith (Map.unionWith(+))
     . fromRight []
     . parse (many guardy) ""
     . unlines . sort . lines
