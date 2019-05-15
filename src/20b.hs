{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

import Text.Parsec hiding (State)
import Text.Pretty.Simple
import Linear

import Data.Graph
import Data.Either
import Data.Function (on)
import Data.Tuple (swap)

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.RWS

import Control.Lens

type Vector = V2 Int

type State = RWS () (UniqueMap Vector Vector) (Set Vector)

newtype UniqueMap k a
  = UniqueMap { fromUniqueMap :: Map k (Set a) }

instance (Ord k, Ord a) => Semigroup (UniqueMap k a) where
  a <> b = UniqueMap $ (Map.unionWith (<>) `on` fromUniqueMap) a b

instance (Ord k, Ord a) => Monoid (UniqueMap k a) where
  mempty = UniqueMap Map.empty

data Branch
  = Branches [[Branch]]
  | Move (V2 Int)
  deriving (Show)

parser = between (char '^') (char '$') branch
  where branch = many $ fmap Branches branches
                    <|> fmap Move direction
        branches = between (char '(') (char ')')
                 $ branch `sepBy` (char '|')
        direction = toVector <$> oneOf "NWSE"
        toVector 'N' = V2 0 (-1)
        toVector 'W' = V2 (-1) 0
        toVector 'S' = V2 0 1
        toVector 'E' = V2 1 0

branch :: Branch -> State ()
branch (Move v) = do
  let modify' vs = zip vs (map (+v) vs)
  vs <- gets $ modify' . Set.toAscList
  tell . UniqueMap . Map.fromList . map (_2 %~ Set.singleton)
    $ vs ++ map swap vs
  put . Set.fromAscList $ map snd vs
branch (Branches bs) = do
  vs <- get
  vs' <- mapM (\b -> put vs >> run b >> get) bs
  put $ mconcat vs'

run :: [Branch] -> State ()
run bs = mapM_ branch bs

bfs start graph = init $ run [start] Set.empty
  where run [] _ = []
        run queue seen =
          let (queue', seen', new) = foldr f ([], seen, []) queue
              f x (q, s, n)
                | x `Set.member` s = (q, s, n)
                | otherwise = ( q ++ Set.toList (graph Map.! x)
                              , Set.insert x s
                              , x : n)
           in new : run queue' seen'

main = do
  parsed <- parse parser "" <$> getContents
  let parsed' = either (error . (show :: ParseError -> String)) id parsed
      graph = fromUniqueMap . snd
            $ execRWS (run parsed') () (Set.singleton (V2 0 0))
  print . sum . map length . drop 1000 $ bfs (V2 0 0) graph

