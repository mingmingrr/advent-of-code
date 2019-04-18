{-# LANGUAGE FlexibleContexts #-}

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Either
import Control.Monad.RWS

import Text.Parsec
import Text.Parsec.Char

type Garden = Set Int
type Rules = Set Garden

plants :: Stream s m Char => ParsecT s u m Garden
plants = many1 (oneOf ".#")
     >>= return . Set.fromList . map fst . filter ((=='#') . snd) . zip [0..]
rule :: Stream s m Char => ParsecT s u m [Garden]
rule = (\ps c -> guard (c == '#') >> return ps)
   <$> (Set.fromList . map (subtract 2) . Set.toList <$> plants)
   <*> (string " => " *> oneOf ".#")
parser :: Stream s m Char => ParsecT s u m (Garden, Rules)
parser = (,) <$> (string "initial state: " *> plants <* many1 newline)
             <*> (Set.fromList . msum <$> many (rule <* spaces))

runStep :: RWS Rules [Garden] Garden ()
runStep = do
  garden <- get
  tell $ return garden
  rules <- ask
  let nums = [(Set.findMin garden - 2) .. (Set.findMax garden + 2)]
      alive n = flip Set.member rules
              . Set.fromList . map fst
              . filter (flip Set.member garden . uncurry (+))
              . zip [-2..2] $ repeat n
      garden' = Set.fromList $ filter alive nums
  modify $ const garden'

runSteps 0 = return ()
runSteps n = runStep >> runSteps (n-1)

main = do
  (garden, rules) <- either (error . show) id . parse parser "" <$> getContents
  let (s, _) = execRWS (runSteps 20) rules garden
  print . sum $ Set.toList s
