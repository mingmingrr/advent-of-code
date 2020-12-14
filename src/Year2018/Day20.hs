{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Year2018.Day20 where

import Text.Megaparsec hiding (State, empty)
import Linear

import Control.Monad.State
import Control.Lens
import System.FilePath

import Data.Tuple
import Data.Maybe
import Data.Void
import Data.Either
import Data.Graph.Inductive
import Data.Set (Set)
import qualified Data.Set as Set

data Regex = Seq [Regex] | Branch [Regex] | Move (V2 Int) deriving Show

parser :: Parsec Void String Regex
parser = between "^" "$" regex where
  regex :: Parsec Void String Regex
  regex = fmap Seq . many $ choice
    [ Branch <$> between "(" ")" (sepBy regex "|")
    , Move . fromJust . flip lookup dirs <$> oneOf ("NWSE" :: String) ]
  dirs = [('N',V2 0 1), ('W',V2 (-1) 0), ('S',V2 0 (-1)), ('E',V2 1 0)]

toEdges :: Regex -> State (Set (V2 Int)) (Set (V2 Int, V2 Int))
toEdges (Branch xs) = state $ \s ->
  let (as, ss) = unzip $ map (flip runState s . toEdges) xs
   in (mconcat as, mconcat ss)
toEdges (Move x) = state $ \s ->
  let f n = Set.mapMonotonic n s
   in (f ((,) <*> (+ x)), f (+ x))
toEdges (Seq xs) = mconcat <$> mapM toEdges xs

part1, part2 :: [(Node, Int)] -> Int
part1 = maximum . map snd
part2 = length . filter ((>= 1000) . snd)

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  let edges = flip evalState (Set.singleton (V2 0 0))
        . toEdges . fromRight undefined $ parse parser "" input
      edges' = Set.toList (Set.map swap edges <> edges)
      (graph :: Gr (V2 Int) (), _) = mkMapGraph
        (edges' ^.. folded . _1) [(a, b, ()) | (a, b) <- edges']
      Just start = lookup (V2 0 0) (map swap (labNodes graph))
  print . part2 $ level start graph

