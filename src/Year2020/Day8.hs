{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Year2020.Day8 where

import Util

import GHC.Generics

import qualified Text.Megaparsec.Char.Lexer as Lex

import Data.Default
import Data.Function
import Data.Functor.Identity
import Data.List

import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Monad.State

import System.FilePath

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Instruction = (String, [String])
type Program = IntMap (Maybe Instruction)

data Registers = Registers
  { _pc :: Int
  , _acc :: Int
  , _mem :: Program
  } deriving (Eq, Show, Generic, Default)
Lens.makeLenses ''Registers

type Console m a = StateT Registers m a

decode :: Monad m => Instruction -> (Int -> Console m a) -> Console m a
decode (inst, [arg]) cont = case inst of
  "nop" -> cont 1
  "jmp" -> cont num
  "acc" -> acc += num >> cont 1
  where num = parseError (Lex.signed mempty Lex.decimal) arg

runConsoleT :: Monad m => Program -> m (Bool, Registers)
runConsoleT prog = evalStateT (run 0) (def & mem .~ prog) where
  run step = pc <+= step >>= \cur ->
    mem . Lens.at cur <<.= Just Nothing >>= \case
      Nothing -> gets (True,)
      Just Nothing -> gets (False,)
      Just (Just inst) -> decode inst run

runConsole :: Program -> (Bool, Registers)
runConsole = runIdentity . runConsoleT

programFromList :: [[String]] -> Program
programFromList = IntMap.fromList . zip [0..] . map uncons

part1, part2 :: [[String]] -> Int
part1 prog = runConsole (programFromList prog) ^. Lens._2 . acc
part2 prog = head [ regs ^. acc
  | (search, replace) <- [("jmp", "nop"), ("nop", "jmp")]
  , index <- findIndices ((== search) . head) prog
  , let prog' = prog & Lens.ix index . Lens._head .~ replace
        (term, regs) = runConsole (programFromList prog')
  , term ]

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  print . part1 . map words $ lines input

