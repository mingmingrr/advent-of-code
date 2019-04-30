{-# LANGUAGE TupleSections, FlexibleContexts, RecordWildCards #-}

import Control.Monad (liftM2)
import Text.Parsec
import Data.Bits

data Sample a = Sample
  { before :: [a]
  , after :: [a]
  , instn :: [a]
  } deriving (Show, Eq)

data Source = Reg | Imm
  deriving (Show, Eq)

type Opcode a = (Source, Source, a -> a -> a)

opcodes :: (Bits a, Ord a, Num a) => [Opcode a]
opcodes =
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

fromBool :: Num a => Bool -> a
fromBool True = 1
fromBool False = 0

parser :: (Stream s m Char, Read a) => ParsecT s u m ([Sample a], [[a]])
parser = liftM2 (,) (many1 sample) (many1 $ lexeme instruction)
  where
    lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
    lexeme = (<*spaces)
    number :: (Stream s m Char, Read a) => ParsecT s u m a
    number = read <$> many1 digit
    instruction :: (Stream s m Char, Read a) => ParsecT s u m [a]
    instruction = number  `sepBy1` char ' '
    list :: (Stream s m Char, Read a) => ParsecT s u m [a]
    list = between (lexeme $ char '[') (lexeme $ char ']')
         $ lexeme number `sepEndBy` lexeme (char ',')
    sample :: (Stream s m Char, Read a) => ParsecT s u m (Sample a)
    sample = do
      lexeme $ string "Before:"
      before <- lexeme list
      instn <- lexeme instruction
      lexeme $ string "After:"
      after <- lexeme list
      return Sample{..}

main = do
  Right a <- parse parser "" <$> getContents
          :: IO (Either ParseError ([Sample Int], [[Int]]))
  print . length . snd $ a
