{-# LANGUAGE TupleSections, FlexibleContexts, RecordWildCards #-}

import Control.Monad.State
import Control.Monad (liftM2)
import Text.Parsec hiding (State)
import Data.Bits
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Regs a = Seq a

(!) = Seq.index

data Sample a = Sample
  { before :: Regs a
  , after :: Regs a
  , instn :: Regs a
  } deriving (Show, Eq)

data Source = Reg | Imm
  deriving (Show, Eq)

type Opcode a = (Source, Source, a -> a -> a)

opcodes :: (Bits a, Ord a, Num a)
        => Seq (Opcode a)
opcodes = Seq.fromList
  [ (Reg, Reg, (+))
  , (Reg, Imm, (+))
  , (Reg, Reg, (*))
  , (Reg, Imm, (*))
  , (Reg, Reg, (.&.))
  , (Reg, Imm, (.&.))
  , (Reg, Reg, (.|.))
  , (Reg, Imm, (.|.))
  , (Reg, Imm, const)
  , (Imm, Imm, const)
  , (Reg, Reg, (fromBool.).(>))
  , (Reg, Imm, (fromBool.).(>))
  , (Imm, Reg, (fromBool.).(>))
  , (Reg, Reg, (fromBool.).(==))
  , (Reg, Imm, (fromBool.).(==))
  , (Imm, Reg, (fromBool.).(==))
  ]

fromNum :: (Integral a, Num b) => a -> b
fromNum = fromInteger . toInteger

getSrc :: (Integral a, Num b)
       => Source -> a -> State (Regs b) b
getSrc Imm i = return $ fromNum i
getSrc Reg i = gets (! fromNum i)

applyOp :: (Bits a, Ord a, Num a, Integral b)
        => Opcode a -> Regs b -> State (Regs a) ()
applyOp op inst = do
  let (x, y, f) = op
  x' <- getSrc x (fromNum $ inst!1)
  y' <- getSrc y (fromNum $ inst!2)
  modify $ Seq.update (fromNum $ inst!3) (f x' y')

fromBool :: Num a => Bool -> a
fromBool True = 1
fromBool False = 0

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

matches Sample{..} = filter m $ toList opcodes
  where m op = execState (applyOp op instn) before == after

main = do
  Right parsed <- parse parser "" <$> getContents
  let samples :: [Sample Int]
      samples = fst parsed
  print . length . filter ((>=3) . length) . map matches $ samples
