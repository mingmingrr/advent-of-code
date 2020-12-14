{-# LANGUAGE CPP #-}

module Year2018.Day23 where

import System.FilePath
import Data.List
import Data.List.Split
import Data.Ord
import Data.SBV
import Data.Maybe
import Text.Read
import Control.Monad
import Control.Arrow

manhattan :: Num a => [a] -> [a] -> a
manhattan = (sum .) . zipWith ((abs .) . (-))

part1, part2 :: [([Integer], Integer)] -> IO Integer
part1 bots = return . fromIntegral . length
  . filter ((<= rad) . manhattan posn . fst) $ bots
  where (posn, rad) = maximumBy (comparing snd) bots
part2 bots = do
  LexicographicResult result <- optimize Lexicographic . goal $
    map (map fromInteger *** fromInteger) bots
  pure . fromJust $ getModelValue "dist" result
  where goal bots = do
          posn <- sIntegers ["x", "y", "z"]
          let inRange (bot, radius) = manhattan bot posn .<= radius
          maximize "count" $ sum [ite (inRange b) (1 :: SInteger) 0 | b <- bots]
          minimize "dist" $ sum (map abs posn)

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  let reader = (init &&& last) . mapMaybe readMaybe . splitOneOf "<>,="
  print =<< part2 (map reader (lines input))

