{-# LANGUAGE TupleSections, FlexibleContexts, RecordWildCards #-}

import Control.Monad.State
import Control.Monad (liftM2)
import Text.Parsec hiding (State)

import Data.Ix (index)
import Data.Function (on)
import Data.Bits
import Data.Foldable
import Data.Maybe

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map

type Regs a = Seq a

(!) = Seq.index

data Sample a = Sample
  { before :: Regs a
  , after :: Regs a
  , instn :: Regs a
  } deriving (Show, Eq)

data Source = Reg | Imm
  deriving (Show, Eq)

data Opcode a = Opcode
  { ident :: Int
  , srcA :: Source
  , srcB :: Source
  , func :: a -> a -> a
  }

instance Eq (Opcode a) where
  (==) = (==) `on` ident

instance Ord (Opcode a) where
  compare = compare `on` ident

opcodes :: (Bits a, Ord a, Num a)
        => Seq (Opcode a)
opcodes = Seq.fromList
  [ Opcode  0 Reg Reg $ (+)
  , Opcode  1 Reg Imm $ (+)
  , Opcode  2 Reg Reg $ (*)
  , Opcode  3 Reg Imm $ (*)
  , Opcode  4 Reg Reg $ (.&.)
  , Opcode  5 Reg Imm $ (.&.)
  , Opcode  6 Reg Reg $ (.|.)
  , Opcode  7 Reg Imm $ (.|.)
  , Opcode  8 Reg Imm $ const
  , Opcode  9 Imm Imm $ const
  , Opcode 10 Reg Reg $ (fromBool.).(>)
  , Opcode 11 Reg Imm $ (fromBool.).(>)
  , Opcode 12 Imm Reg $ (fromBool.).(>)
  , Opcode 13 Reg Reg $ (fromBool.).(==)
  , Opcode 14 Reg Imm $ (fromBool.).(==)
  , Opcode 15 Imm Reg $ (fromBool.).(==)
  ]

getSrc :: (Integral a, Num b)
       => Source -> a -> State (Regs b) b
getSrc Imm i = return $ fromIntegral i
getSrc Reg i = gets (! fromIntegral i)

applyOp :: (Bits a, Ord a, Num a, Integral b)
        => Opcode a -> Regs b -> State (Regs a) ()
applyOp Opcode{..} inst = do
  a <- getSrc srcA (fromIntegral $ inst!1)
  b <- getSrc srcB (fromIntegral $ inst!2)
  modify $ Seq.update (fromIntegral $ inst!3) (func a b)

fromBool :: Num a => Bool -> a
fromBool = fromIntegral . index (False, True)

parser :: (Stream s m Char, Read a)
       => ParsecT s u m ([Sample a], [Regs a])
parser = liftM2 (,) (many1 sample) (many1 $ lexeme instruction)
  where
    lexeme = (<*spaces)
    number = read <$> many1 digit
    instruction = Seq.fromList <$> (number  `sepBy1` char ' ')
    list = Seq.fromList
       <$> between (lexeme $ char '[') (lexeme $ char ']')
           (lexeme number `sepEndBy` lexeme (char ','))
    sample = do
      lexeme $ string "Before:"
      before <- lexeme list
      instn <- lexeme instruction
      lexeme $ string "After:"
      after <- lexeme list
      return Sample{..}

matches :: (Bits a, Ord a, Integral a)
        => Opcode a -> Sample a -> Bool
matches op Sample{..} = execState (applyOp op instn) before == after

filterOpcode :: (Bits a, Ord a, Integral a)
             => Sample a -> Map a [Opcode a] -> Map a [Opcode a]
filterOpcode samp@Sample{..} = Map.alter
  (return . filter (`matches` samp) . fromMaybe (toList opcodes))
  (fromIntegral $ instn!0)

newtype OpPair a = OpPair { fromOpPair :: (a, Opcode a) }

instance Eq (OpPair a) where
  (==) = (==) `on` (snd . fromOpPair)

toOpPair (x, y) = map (OpPair . (x,)) y

uniques :: Eq a => [[a]] -> [[a]]
uniques [] = [[]]
uniques [[]] = []
uniques [x] = map pure x
uniques (a:as) = do
  x <- a
  let as' = map (filter (/= x)) as
  guard $ all ((/= 0) . length) as'
  map (x:) (uniques as')

main = do
  Right parsed <- parse parser "" <$> getContents
  let samples :: [Sample Int]
      samples = fst parsed
      instructions = snd parsed
      opcodes' = Map.fromList . map fromOpPair . head
               . uniques . map toOpPair . Map.assocs
               . foldr filterOpcode Map.empty $ samples
      runOp inst = applyOp (opcodes' Map.! (inst!0)) inst
      runAll = sequence $ map runOp instructions
  print $ execState runAll (Seq.fromList [0, 0, 0, 0]) ! 0

