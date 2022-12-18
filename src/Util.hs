{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Util where

import Debug.Trace

import Numeric

import qualified Linear as Lin
import Linear.V2

import Text.Pretty.Simple
import qualified Text.Megaparsec as Par
import qualified Text.Megaparsec.Char as Par

import Data.String.Here
import Data.Data
import Data.Generics
import Data.Char
import Data.Function
import Data.Maybe
import Data.Ord
import Data.Void
import Data.Monoid
import Data.Tuple
import Data.List
import Data.List.Split
import Data.List.Extra (groupOn)

import qualified Data.PQueue.Prio.Min as Queue
import qualified Data.Bimap as Bimap
import qualified Data.Tree as Tree
import qualified Data.Graph.Inductive as Graph
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TextL
import Data.Map.Syntax

import Control.Applicative
import Control.Monad
import qualified Control.Lens as Lens
import Control.Lens.Operators hiding ((??))

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

rotateCW, rotateCCW :: Num a => V2 a -> V2 a
rotateCW (V2 x y) = V2 y (-x)
rotateCCW (V2 x y) = V2 (-y) x

labelGrid :: (Num n, Enum n) => [[a]] -> [(V2 n, a)]
labelGrid = concat . zipWith rowWise [0..]
  where rowWise r = zipWith (colWise r) [0..]
        colWise r c x = (V2 r c, x)

blocksOf :: Int -> Int -> [[a]] -> [[[[a]]]]
blocksOf row col = map (transpose . map (chunksOf col)) . chunksOf row

dijkstra :: (Ord pos, Ord cost, Num cost, Show cost)
         => (pos -> [(cost, pos)]) -> pos
         -> [(cost, [pos])]
dijkstra neighbors start = go Set.empty queue0 where
  queue0 = Queue.singleton 0 [start]
  go seen queue = case Queue.minViewWithKey queue of
    -- Just ((cost, _), _) | traceShow cost False -> undefined
    Just ((cost, path@(pos:_)), queue') -> if Set.member pos seen
      then go seen queue'
      else let seen' = Set.insert pos seen
               queue'' = Queue.union queue' $ Queue.fromList
                 [(cost+cost', pos':path) | (cost', pos') <- neighbors pos]
            in (cost, path) : go seen' queue''
    Just _ -> error "dijkstra: invalid queue"
    Nothing -> []

orthogonal, diagonal :: Num a => [V2 a]
orthogonal = [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]
diagonal = [V2 1 1, V2 (-1) 1, V2 1 (-1), V2 (-1) (-1)]

bounds :: (Foldable t, Applicative f, Ord a) => t (f a) -> (f a, f a)
bounds = (,) <$> foldr1 (liftA2 min) <*> foldr1 (liftA2 max) 

inBounds :: (Foldable f, Ord a, Num a, Num (f a))
         => f a -> f a -> f a -> Bool
inBounds low high pos
  = getAll (foldMap (All . (>= 0)) (pos - low))
 && getAll (foldMap (All . (>= 0)) (high - pos))

allInBounds
  :: ( Lens.Each s (t a) a a, Num s
     , Foldable t, Applicative t, Enum a, Ord a )
  => t a -> t a -> [t a]
allInBounds x y = map (\x -> Lens.set (Lens.partsOf Lens.each) x 0)
  . foldr (liftA2 (:)) [[]] $ liftA2 enumFromTo x y

displayGrid :: [(V2 Int, Char)] -> String
displayGrid xs = unlines [[lookup (V2 r c) | c <- [c1..c2]] | r <- [r1..r2]] where
  (V2 r1 c1, V2 r2 c2) = bounds (map fst xs)
  lookup x = Map.fromList xs Map.!? x ?? ' '

neighbors :: Num a => [a] -> [a -> Bool] -> a -> [a]
neighbors edges checkers pos =
  [pos' | diff <- edges, let pos' = pos + diff, all ($ pos') checkers]

manhattan :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
manhattan x y = getSum . foldMap Sum . abs $ x - y

counter :: (Ord a, Num b) => [a] -> Map.Map a b
counter = Map.fromListWith (+) . map (, 1)

common :: (Ord a, Num b, Ord b) => [a] -> [(a, b)]
common = sortOn (\(a, b) -> (Down b, a)) . Map.toList . counter

swapCase :: Char -> Char
swapCase x = if isLower x then toUpper x else toLower x

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

readBase :: (Num a, Integral b) => a -> [b] -> a
readBase b = foldl (\n x -> n * b + fromIntegral x) 0

readOnlyNums :: (Read a, Num a) => String -> [a]
readOnlyNums = map read . filter (isDigit . head) . groupOn isDigit

readOnlyNums' :: (Read a, Num a) => String -> [a]
readOnlyNums' = map read . filter (pred . head) . groupOn pred where
  pred c = isDigit c || c == '-'

type ParserSimple = Par.Parsec Void String

parserSimple :: ParserSimple a -> ParserSimple a
parserSimple = id

parseError
  :: (Par.TraversableStream s, Par.VisualStream s, Par.ShowErrorComponent e)
  => Par.Parsec e s a -> s -> a
parseError p = either (error . Par.errorBundlePretty) id . Par.parse p ""

traceWith :: (a -> String) -> a -> a
traceWith f x = trace (f x) x

traceShowWith :: Show b => (a -> b) -> a -> a
traceShowWith f x = traceShow (f x) x

splitOneOf' :: Eq a => [a] -> [a] -> [[a]]
splitOneOf' xs = filter (not . null) . splitOneOf xs

readsMaybe :: ReadS a -> String -> Maybe a
readsMaybe r = fmap fst . listToMaybe . r

pInteger :: (Par.MonadParsec e s m, Par.Token s ~ Char, MonadPlus m, Num a, Eq a) => m a
pInteger = Par.some Par.digitChar >>= maybe mzero pure . readsMaybe readDec

pOctal :: (Par.MonadParsec e s m, Par.Token s ~ Char, MonadPlus m, Num a, Eq a) => m a
pOctal = Par.some Par.hexDigitChar >>= maybe mzero pure . readsMaybe readOct

pHexadecimal :: (Par.MonadParsec e s m, Par.Token s ~ Char, MonadPlus m, Num a, Eq a) => m a
pHexadecimal = Par.some Par.hexDigitChar >>= maybe mzero pure . readsMaybe readHex

pBinary :: (Par.MonadParsec e s m, Par.Token s ~ Char, MonadPlus m, Num a, Eq a) => m a
pBinary = Par.some Par.binDigitChar >>= maybe mzero pure . readsMaybe
  (readInt 2 (`elem` "01") (read . pure))

pBetween :: (Par.MonadParsec e s m, Par.Token s ~ c) => [c] -> m a -> m a
pBetween [x, y] = Par.between (Par.single x) (Par.single y)
pBetween _ = error "pBetween: invalid tokens"

pIdentifierWith :: (Par.MonadParsec e s m, Par.Token s ~ Char) => m Char -> m Char -> m String
pIdentifierWith start middle = (:) <$> start <*> Par.many middle

pIdentifier :: (Par.MonadParsec e s m, Par.Token s ~ Char) => m String
pIdentifier = pIdentifierWith Par.letterChar Par.alphaNumChar

tuplify :: Lens.Simple Lens.Each b a => [a] -> b
tuplify xs = undefined & Lens.partsOf Lens.each .~ xs

uncurry' :: (a -> a -> b) -> [a] -> b
uncurry' f [x,y] = f x y

uncurry3' :: (a -> a -> a -> b) -> [a] -> b
uncurry3' f [x,y,z] = f x y z

runMap' :: Ord k => MapSyntaxM k v a -> Map.Map k v
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

fromEdges :: (Graph.Graph graph, Ord node)
  => [(node, node, edge)] -> (Bimap.Bimap Int node, graph node edge)
fromEdges edges = (bimap, Graph.mkGraph (Bimap.assocs bimap) edges')
  where bimap = Bimap.fromList . zip [0..] . nubSet $
          edges ^.. Lens.folded . (Lens._1 <> Lens._2)
        edges' = [(bimap Bimap.!> a, bimap Bimap.!> b, c) | (a,b,c) <- edges]

fromGrid :: (Graph.Graph graph, Ord node)
  => [V2 Int] -> [[node]] -> (Bimap.Bimap Int (V2 Int, node), graph (V2 Int, node) (node, node))
-- fromGrid :: [V2 Int] -> [[Char]] -> (Bimap.Bimap Int (V2 Int), Graph.Gr (V2 Int) (Char, Char))
fromGrid neighbors nodes = fromEdges edges where 
  grid = Map.fromList (labelGrid nodes)
  edges = [ ((a, b), (c, d), (b, d))
    | (a, b) <- Map.toList grid
    , c <- map (+ a) neighbors
    , Just d <- [grid Map.!? c] ]

xdffWith' :: Graph.Graph gr
  => Graph.CFun a b [Graph.Node] -> Graph.CFun a b c
  -> [Graph.Node] -> gr a b -> [Tree.Tree c]
xdffWith' _ _ [] _ = []
xdffWith' _ _ _ g | Graph.isEmpty g = []
xdffWith' d f (v:vs) g = case Graph.match v g of
  (Nothing, _) -> xdffWith' d f vs g
  (Just c, _)-> Tree.Node (f c) ts : ts'
    where ts = xdffWith' d f (d c) g
          ts' = xdffWith' d f vs g

compass :: Map.Map Char (V2 Int)
compass = runMap' $ do
  'E' ## V2 0 1
  'W' ## V2 0 (-1)
  'S' ## V2 1 0
  'N' ## V2 (-1) 0
  'R' ## V2 0 1
  'L' ## V2 0 (-1)
  'D' ## V2 1 0
  'U' ## V2 (-1) 0

directions :: Map.Map String (V2 Int)
directions = runMap' $ do
  "R" ## V2 0 1
  "L" ## V2 0 (-1)
  "D" ## V2 1 0
  "U" ## V2 (-1) 0
  "E" ## V2 0 1
  "W" ## V2 0 (-1)
  "S" ## V2 1 0
  "N" ## V2 (-1) 0
  "NE" ## V2 (-1) 1
  "SE" ## V2 1 1
  "NW" ## V2 (-1) (-1)
  "SW" ## V2 1 (-1)

brackets :: Bimap.Bimap Char Char
brackets = Bimap.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

using :: (Data a, Typeable a) => String -> TH.Q a -> TH.Q a
using mod = (>>= everywhereM (mkM (resolve mod))) where
  resolve mod = \case
    exp@(TH.UnboundVarE name) -> maybe exp TH.VarE <$>
      TH.lookupValueName (mod <> "." <> TH.nameBase name)
    exp -> pure exp

(??) :: Maybe a -> a -> a
(??) = flip fromMaybe
infixl 9 ??

(!??) :: (Lens.At a, Num (Lens.IxValue a))
  => a -> Lens.Index a -> Lens.IxValue a
m !?? n = fromMaybe 0 (m ^. Lens.at n)
infixl 9 !??

contexts :: Graph.Graph gr => gr a b -> [Graph.Context a b]
contexts graph = map (Graph.context graph) (Graph.nodes graph)

zipper :: [a] -> [([a], a, [a])]
zipper [] = []
zipper (x:xs) = ([], x, xs) : [ (x:a, b, c) | (a, b, c) <- zipper xs ]

