{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

import Text.Parsec
import Text.Pretty.Simple
import Linear

import Data.Function (on)

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

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

branch = many $ fmap Branches branches
            <|> fmap Move direction

branches = between (char '(') (char ')')
         $ branch `sepBy` (char '|')

direction = toVector <$> oneOf "NWSE"
  where toVector 'N' = V2 0 (-1)
        toVector 'W' = V2 (-1) 0
        toVector 'S' = V2 0 1
        toVector 'E' = V2 1 0

main = getContents >>= Text.putStrLn . Text.take 20000 . pShow . parse parser ""

