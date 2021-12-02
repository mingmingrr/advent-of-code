{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Year2021.Day2 where

import System.FilePath
import Control.Monad.State
import Control.Lens

data Submarine = Submarine
  { _horiz :: Int
  , _vert :: Int
  , _aim :: Int
  } deriving (Eq, Show, Ord)
makeLenses ''Submarine

part1, part2 :: Int -> Char -> State Submarine ()
part1 n 'f' = horiz += n
part1 n 'd' = vert -= n
part1 n 'u' = vert += n
part2 n 'f' = horiz += n >> join (uses aim ((vert +=) . (* n)))
part2 n 'd' = aim -= n
part2 n 'u' = aim += n

main = do
  readFile (replaceExtension __FILE__ ".in") >>= print
    . ((*) <$> _horiz <*> abs . _vert) . flip execState (Submarine 0 0 0)
    . mapM_ (\(words -> [c:_, n]) -> part2 (read n) c) . lines

