{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

import Data.Char (ord)
import Data.Maybe
import Data.Either
import Data.Function

import Text.Parsec
import Text.Parsec.Char

import Control.Monad.RWS
import Control.Monad
import Control.Lens

data Node a = Node
  { _ins :: Set a
  , _outs :: Set a
  }
makeLenses ''Node

(>:>) :: Ord a => [a] -> [a] -> Node a
a >:> b = (Node `on` Set.fromList) a b

instance Show a => Show (Node a) where
  show n = s ins ++ " >:> " ++ s outs
    where s lens = show . Set.toList $ n ^. lens

instance Ord a => Semigroup (Node a) where
  a <> b = let ap n = on (<>) (^.n) a b
            in Node (ap ins) (ap outs)

instance Ord a => Monoid (Node a) where
  mempty = Node Set.empty Set.empty

type Graph a = Map a (Node a)

toGraph :: Ord a => [(a, a)] -> Graph a
toGraph = foldr f Map.empty
  where f (x, y) = insert ins x y . insert outs y x
        insert lens x y = flip Map.alter y
                        $ Just . (lens %~ Set.insert x)
                        . fromMaybe mempty

edge = (,) <$> (string "Step " *> letter <* string " must be finished")
           <*> (string " before step " *> letter <* string " can begin.")

type Time = Int

data Assembly a = Assembly
  { _time :: Time
  , _graph :: Graph a
  , _queue :: Graph a
  , _workers :: Map Time [(a, Node a)]
  } deriving (Show)
makeLenses ''Assembly

start = Assembly
  { _time = 0
  , _graph = Map.empty
  , _queue = Map.empty
  , _workers = Map.empty
  }

data Config a = Config
  { _getTime :: a -> Time
  , _numWorkers :: Int
  }
makeLenses ''Config

startAssembly = do
  (queue', graph') <- uses graph $ Map.partition (^.ins.to Set.null)
  queue %= mappend queue'
  graph .= graph'
  runAssembly

runAssembly = do
  clearWorkers
  clearQueue
  checkLeftovers

checkLeftovers = do
  w <- use workers
  if null w
     then return ()
     else do
       (t, _) <- uses workers Map.findMin
       time .= t
       runAssembly

clearNode a n = do
  g <- uses graph $ Map.adjust (ins %~ Set.delete a) n
  let n' = g Map.! n
  if not. null $ n' ^. ins
    then graph .= g
    else do
      queue %= Map.insert n n'
      graph %= Map.delete n

clearWorkers = do
  let clear (a, node) = tell [a] >> mapM_ (clearNode a) (node ^. outs)
  (t, w) <- (,) <$> use time <*> use workers
  maybe (return ()) (mapM_ clear) (Map.lookup t w)
  workers %= Map.delete t

clearQueue = do
  (getTime', numWorks) <- (,) <$> view getTime <*> view numWorkers
  (time', numBusy) <- (,) <$> use time <*> uses workers Map.size
  (workers', queue') <- uses queue $ Map.splitAt (numWorks - numBusy)
  queue .= queue'
  let mapping (a, node) = (time' + getTime' a, [(a, node)])
      workers'' = Map.fromList . map mapping . Map.toList $ workers'
  workers %= Map.unionWith (++) workers''

main = do
  edges <- either (error . show) id
         . parse (many (edge <* spaces)) ""
       <$> getContents
  let config = Config (\c -> ord c - ord 'A' + 1 + 60) 1
      graph' = toGraph edges
      (s, w) = execRWS startAssembly config (start & graph .~ graph')
  putStrLn w

