{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import Text.Parsec hiding (State)
import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Data.Either
import Data.Maybe
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

type Computation = RWS Instructions [Int] Computer

newtype Instructions = Instructions
  { getInstructions :: Map Int (Computation ()) }

data Computer = Computer
  { _regs :: Map String Integer
  , _pcreg :: Int
  }
makeLenses ''Computer

instance Show Computer where
  show x = show (x^.pcreg) ++ "<:>" ++ show (x^.regs)

regInst s f = do
  string s *> spaces
  reg <- many1 letter
  return $ do
    regs %= Map.alter (fmap f . flip mplus (pure 0)) reg
    pcreg += 1

jmpInst s f = do
  string s *> spaces
  reg <- option "a" (many1 letter <* char ',' <* spaces)
  offset <- read . dropWhile (=='+')
        <$> liftM2 (:) (oneOf "+-") (many1 digit)
  return $ do
    val <- uses regs (Map.findWithDefault 0 reg)
    pcreg += if f val then offset else 1

instruction :: Parsec String () (Computation ())
instruction = try (regInst "hlf" (`div` 2))
          <|> try (regInst "tpl" (* 3))
          <|> try (regInst "inc" (+ 1))
          <|> try (jmpInst "jmp" (const True))
          <|> try (jmpInst "jie" (even))
          <|> jmpInst "jio" (== 1)

instructions = instruction `sepEndBy` newline

runComputation i = execRWS running (Instructions . Map.fromList $ zip [1..] i) initial
  where initial = Computer { _regs = Map.singleton "a" 1, _pcreg = 1 }
        running = liftM2 Map.lookup (use pcreg) (asks getInstructions)
              >>= maybe (return ()) (\x -> use pcreg >>= tell.pure >> x >> running)

main = interact $ show . Map.findWithDefault 0 "b" . _regs . fst
                . runComputation . either (error . show) id . parse instructions ""

