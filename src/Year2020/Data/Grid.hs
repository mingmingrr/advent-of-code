{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Year2020.Data.Grid where

import Prelude hiding (length)

import Data.Foldable hiding (length)

import Control.Parallel.Strategies
import qualified Control.Lens as Lens
import Control.Lens.Operators

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

newtype Grid a = Grid { getGrid :: Seq (Seq a) }
  deriving (Eq, Show, Ord, Functor)

parMapWithKey :: (NFData b) => ((Int, Int) -> a -> b) -> Grid a -> Grid b
parMapWithKey f xs = Grid . Seq.fromList . parMap rdeepseq id
  . zipWith (\x -> Seq.mapWithIndex (\y v -> f (x, y) v)) [0..]
  . toList $ getGrid xs

fromLists :: (Foldable f1, Foldable f2) => f1 (f2 a) -> Grid a
fromLists  = Grid . Seq.fromList . map (Seq.fromList . toList) . toList

toLists :: Grid a -> [[a]]
toLists = map toList . toList . getGrid

mkLens2 :: ( Lens.Ixed m, Lens.Ixed (Lens.IxValue m)
           , Lens.Simple Lens.Field1 s (Lens.Index m)
           , Lens.Simple Lens.Field2 s (Lens.Index (Lens.IxValue m))
           ) => s -> Lens.Traversal' m (Lens.IxValue (Lens.IxValue m))
mkLens2 p = Lens.ix (p ^. Lens._1) . Lens.ix (p ^. Lens._2)

(!) :: ( Lens.Simple Lens.Field1 s Int
        , Lens.Simple Lens.Field2 s Int
        ) => Grid a -> s -> a
xs ! p = getGrid xs ^. Lens.singular (mkLens2 p)

(!?) :: ( Lens.Simple Lens.Field1 s Int
        , Lens.Simple Lens.Field2 s Int
        ) => Grid a -> s -> Maybe a
xs !? p = getGrid xs ^? mkLens2 p

height, width :: Grid a -> Int
height = Seq.length . getGrid
width = maybe 0 Seq.length . Seq.lookup 0 . getGrid

