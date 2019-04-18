{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import Data.List.PointedList (PointedList)
import qualified Data.List.PointedList.Circular as PList
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad.RWS
import Control.Lens

import Text.Parsec
import Text.Parsec.Char

data GameState = GameState
  { _marbles :: PointedList Integer
  , _turn :: Integer
  , _scores :: Map Integer Integer
  } deriving (Show)
makeLenses ''GameState

start = GameState
  { _marbles = PList.singleton 0
  , _turn = 0
  , _scores = Map.empty
  }

data GameEnv = GameEnv
  { _players :: Integer
  , _maxTurn :: Integer
  } deriving (Show)
makeLenses ''GameEnv

parser :: Stream s m Char => ParsecT s u m GameEnv
parser = GameEnv
     <$> ( number <* str "players;" )
     <*> ( str "last marble is worth" *> number <* str "points" )
  where number = read <$> many1 digit
        str s = spaces *> string s <* spaces

type Game = RWS GameEnv [GameState] GameState

runTurn :: Game ()
runTurn = do
  modify $ turn %~ (+1)
  turn' <- (^.turn) <$> get
  case turn' `mod` 23 of
    0 -> do
      players <- (^.players) <$> ask
      modify $ marbles %~ PList.moveN (-7)
      points <- (+turn') . (^. marbles . PList.focus) <$> get
      modify $ marbles %~ fromJust . PList.deleteRight
      modify $ scores %~ Map.alter
        (Just . maybe points (+points))
        ((turn'-1) `mod` players)
    _ -> modify $ marbles %~ (PList.insertRight turn' .  PList.next)
  get >>= tell . return

runGame :: Game ()
runGame = do
  turn <- (^.turn) <$> get
  maxTurn <- (^.maxTurn) <$> ask
  if turn == maxTurn
     then return ()
     else runTurn >> runGame

main = do
  Right env <- parse parser "" <$> readFile "9.in"
  let env' = env { _maxTurn = 100 * _maxTurn env }
      (s, w) = execRWS runGame env' start
  print . maximum . Map.elems $ s ^. scores
