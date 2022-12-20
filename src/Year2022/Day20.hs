{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Year2022.Day20 where

import Util
import Safe
import Data.Foldable
import Data.Bits
import Data.FingerTree (FingerTree, Measured)
import qualified Data.FingerTree as FT
import System.FilePath

data Mask = Mask { size :: Int, mask :: Integer } deriving Show
instance Semigroup Mask where Mask a b <> Mask c d = Mask (a + c) (b .|. d)
instance Monoid Mask where mempty = Mask 0 0
data Node a = Node { index :: Int, node :: a } deriving (Eq, Ord)
instance Show a => Show (Node a) where show = show . node
type Seq a = FingerTree Mask (Node a)
instance Ord a => Measured Mask (Node a) where measure (Node n _) = Mask 1 (bit n)

run :: Seq Int -> (Int,Int) -> Seq Int
run xs (k,v) = ls FT.>< Node k v FT.<| rs where
  (as, FT.viewl -> n FT.:< bs) = FT.split (flip testBit k . mask) xs
  i = (size (FT.measure as) + v) `mod` (size (FT.measure xs) - 1)
  (ls, rs) = FT.split ((> i) . size) (as FT.>< bs)

part1, part2 :: (Int, Int)
part1 = (1, 1)
part2 = (10, 811589153)

main = readFile (replaceExtension __FILE__ ".in") >>= \(readOnlyNums' -> xs) -> print
  . sum . flip map [1000, 2000, 3000]
  . ((.) <$> (!!) <*> flip mod . length)
  . map node . toList . uncurry (flip (FT.><))
  . FT.split ((`testBit` elemIndexJust 0 xs) . mask)
  . (foldl' run <$> FT.fromList . map (uncurry Node) <*> concat . replicate times)
  . zip [0..] . map (* encrypt) $ xs
  where (times, encrypt) = part2
