{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day17 where

import Util

import GHC.TypeLits as TypeLits

import qualified Linear.V as Lin
import Linear.V2
import Linear.V3
import Linear.V4

import Data.Proxy
import Data.Semigroup
import Data.Function
import Data.List.Extra

import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Monad

import System.FilePath

import qualified Data.Set as Set

type Part1 = V3
type Part2 = V4

around :: forall f a n .
  ( Lin.Finite f, KnownNat n, n ~ Lin.Size f
  , Lens.Simple Lens.Each (f a) a
  , Eq (f a), Num a, Num (f a) ) => [f a]
around = map (\x -> 0 & Lens.partsOf Lens.each .~ x) $
  replicateM (fromInteger (TypeLits.natVal (Proxy :: Proxy n))) [-1,0,1]

life :: Endo [Part2 Int]
life = Endo $ \xs ->
  let active = (`Set.member` Set.fromList xs)
      filt x = ns == 3 || active x && ns == 4 where
        ns = length (neighbors around [active] x)
   in filter filt . nubOrd $ xs >>= neighbors around []

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . length . appEndo (stimes 6 life)
    . map (flip (Lens.set _xy) 0 . fst)
    . filter ((=='#') . snd) . labelGrid $ lines input

