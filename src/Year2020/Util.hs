{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Year2020.Util where

import Debug.Trace

import Numeric

import Linear.V2

import Text.Pretty.Simple
import Text.Read (readMaybe)
import Text.Megaparsec as Par
import Text.Megaparsec.Char as Par

import Data.String.Here
import Data.String
import Data.Maybe
import Data.Either
import Data.Ord
import Data.Void
import Data.Monoid
import Data.List
import Data.List.Split

import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as Queue
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Graph.Inductive (Graph)
import qualified Data.Graph.Inductive as Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Syntax
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as TextL

import Control.Monad
import qualified Control.Lens as Lens
import Control.Lens.Operators

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH

rotateCW, rotateCCW :: Num a => V2 a -> V2 a
rotateCW (V2 x y) = V2 (-y) x
rotateCCW (V2 x y) = V2 y (-x)

labelGrid :: (Num n, Enum n) => [[a]] -> [(V2 n, a)]
labelGrid = concat . zipWith rowWise [0..]
  where rowWise r = zipWith (colWise r) [0..]
        colWise r c x = (V2 r c, x)

blocksOf :: Int -> Int -> [[a]] -> [[[[a]]]]
blocksOf row col = map (transpose . map (chunksOf col)) . chunksOf row

dijkstra :: (Ord pos, Ord cost, Num cost)
         => (pos -> [(cost, pos)]) -> pos
         -> [(cost, [pos])]
dijkstra neighbors start = go Set.empty queue0 where
  queue0 = Queue.singleton 0 [start]
  go seen queue = case Queue.minViewWithKey queue of
    Just ((cost, path@(pos:_)), queue') -> if Set.member pos seen
      then go seen queue'
      else let seen' = Set.insert pos seen
               queue'' = Queue.union queue' $ Queue.fromList
                 [(cost+cost', pos':path) | (cost', pos') <- neighbors pos]
            in (cost, path) : go seen' queue''
    Nothing -> []

adjacent, diagonal :: Num a => [V2 a]
adjacent = [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]
diagonal = [V2 1 1, V2 (-1) 1, V2 1 (-1), V2 (-1) (-1)]

inBounds :: (Foldable f, Ord a, Num a, Num (f a))
         => f a -> f a -> f a -> Bool
inBounds low high pos
  = getAll (foldMap (All . (>= 0)) (pos - low))
 && getAll (foldMap (All . (>= 0)) (high - pos))

neighbors :: (Foldable f, Ord a, Num a, Num (f a))
          => [f a] -> [f a -> Bool] -> f a -> [f a]
neighbors edges checkers pos =
  [pos' | diff <- edges, let pos' = pos + diff, all ($ pos') checkers]

manhattan :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
manhattan x y = getSum . foldMap Sum . abs $ x - y

counter :: (Ord a, Num b) => [a] -> Map.Map a b
counter = Map.fromListWith (+) . map (, 1)

class Variadic a r t | t -> r where
  liftVariadic :: ([a] -> r) -> t
instance Variadic a r r where
  liftVariadic f = f []
instance (a ~ a', Variadic a r t) => Variadic a r (a' -> t) where
  liftVariadic f h = liftVariadic (f . (h:))

variadicList :: Variadic a [a] t => t
variadicList = liftVariadic id

readNumeric :: (Read a, Num a) => String -> a
readNumeric = read

readIntegral :: (Num a, Read a) => String -> a
readIntegral = read

readInteger :: String -> Integer
readInteger = read

type ParserSimple a = Par.Parsec Void String a

parserSimple :: ParserSimple a -> ParserSimple a
parserSimple = id

parseError :: ParserSimple a -> String -> a
parseError p = either (error . Par.errorBundlePretty) id . Par.parse p ""

traceShowWith :: Show b => (a -> b) -> a -> a
traceShowWith f x = traceShow (f x) x

splitOneOf' :: Eq a => [a] -> [a] -> [[a]]
splitOneOf' xs = filter (not . null) . splitOneOf xs

minimumOn, maximumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy (comparing f)
maximumOn f = maximumBy (comparing f)

readsMaybe :: ReadS a -> String -> Maybe a
readsMaybe r = fmap fst . listToMaybe . r

pInteger :: (Par.MonadParsec e s m, Token s ~ Char, MonadPlus m, Num a, Eq a) => m a
pInteger = some Par.digitChar >>= maybe mzero pure . readsMaybe readDec

pOctal :: (Par.MonadParsec e s m, Token s ~ Char, MonadPlus m, Num a, Eq a) => m a
pOctal = some Par.hexDigitChar >>= maybe mzero pure . readsMaybe readOct

pHexadecimal :: (Par.MonadParsec e s m, Token s ~ Char, MonadPlus m, Num a, Eq a) => m a
pHexadecimal = some Par.hexDigitChar >>= maybe mzero pure . readsMaybe readHex

pBinary :: (Par.MonadParsec e s m, Token s ~ Char, MonadPlus m, Num a, Eq a) => m a
pBinary = some Par.binDigitChar >>= maybe mzero pure . readsMaybe
  (readInt 2 (`elem` "01") (read . pure))

pBetween :: (Par.MonadParsec e s m, Token s ~ c) => [c] -> m a -> m a
pBetween [x, y] = Par.between (Par.single x) (Par.single y)

pIdentifierWith :: (Par.MonadParsec e s m, Token s ~ Char) => m Char -> m Char -> m String
pIdentifierWith start middle = (:) <$> start <*> many middle

pIdentifier :: (Par.MonadParsec e s m, Token s ~ Char) => m String
pIdentifier = pIdentifierWith Par.letterChar Par.alphaNumChar

tuplify :: Lens.Simple Lens.Each b a => [a] -> b
tuplify xs = undefined & Lens.partsOf Lens.each .~ xs

runMap' :: Ord k => MapSyntaxM k v a -> Map k v
runMap' = either (error "invalid MapSyntax") id . runMap

between :: Ord a => a -> a -> a -> Bool
between low high value = low <= value && value <= high

paragraphs :: String -> [[String]]
paragraphs = splitWhen null . lines

r :: TH.QuasiQuoter
r = hereLit

pShow' :: Show a => a -> String
pShow' = TextL.unpack . pShow

nubSet :: Ord a => [a] -> [a]
nubSet = Set.toList . Set.fromList

fromEdges :: (Graph gr, Ord a) => [(a, a, b)] -> (Bimap Int a, gr a b)
fromEdges edges = (bimap, Graph.mkGraph (Bimap.assocs bimap) edges')
  where bimap = Bimap.fromList . zip [0..] . nubSet $
          edges ^.. Lens.folded . (Lens._1 <> Lens._2)
        edges' = [(bimap Bimap.!> a, bimap Bimap.!> b, c) | (a,b,c) <- edges]

xdffWith' :: Graph gr
  => Graph.CFun a b [Graph.Node] -> Graph.CFun a b c
  -> [Graph.Node] -> gr a b -> [Tree.Tree c]
xdffWith' _ _ [] _ = []
xdffWith' _ _ _ g | Graph.isEmpty g = []
xdffWith' d f (v:vs) g = case Graph.match v g of
  (Nothing, _) -> xdffWith' d f vs g
  (Just c, _)-> Tree.Node (f c) ts : ts'
    where ts = xdffWith' d f (d c) g
          ts' = xdffWith' d f vs g

