{-# LANGUAGE FlexibleContexts #-}

module Year2020.Data.Cyclic where

import Prelude hiding (length)

import qualified Control.Lens as Lens
import Control.Lens.Operators

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

newtype Cyclic a = Cyclic { getCyclic :: Seq a }
  deriving (Eq, Show, Ord, Functor)

(!) :: Cyclic a -> Int -> a
xs ! p = Seq.index (getCyclic xs) (modSize xs p)

modSize :: Cyclic a -> Int -> Int
modSize xs p = mod p (length xs)

length :: Cyclic a -> Int
length = Seq.length . getCyclic

newtype Cyclic2 a = Cyclic2 { getCyclic2 :: Seq (Seq a) }
  deriving (Eq, Show, Ord, Functor)

modSize2 :: ( Lens.Simple Lens.Field1 s Int
            , Lens.Simple Lens.Field2 s Int
            ) => Cyclic2 a -> s -> s
modSize2 xs p = p & Lens._1 %~ (`mod` height xs)
                  & Lens._2 %~ (`mod` width xs)

mkLens2 :: ( Lens.Ixed m, Lens.Ixed (Lens.IxValue m)
           , Lens.Simple Lens.Field1 s (Lens.Index m)
           , Lens.Simple Lens.Field2 s (Lens.Index (Lens.IxValue m))
           ) => s -> Lens.Traversal' m (Lens.IxValue (Lens.IxValue m))
mkLens2 p = Lens.ix (p ^. Lens._1) . Lens.ix (p ^. Lens._2)

(!!) :: ( Lens.Simple Lens.Field1 s Int
        , Lens.Simple Lens.Field2 s Int
        ) => Cyclic2 a -> s -> a
xs !! p = getCyclic2 xs ^. Lens.singular (mkLens2 (modSize2 xs p))

height, width :: Cyclic2 a -> Int
height = Seq.length . getCyclic2
width = maybe 0 Seq.length . Seq.lookup 0 . getCyclic2

