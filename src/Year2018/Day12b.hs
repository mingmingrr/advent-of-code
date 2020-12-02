{-# LANGUAGE FlexibleContexts #-}

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Either
import Control.Monad.RWS

import Text.Parsec
import Text.Parsec.Char

type Garden = [Integer]
type Rules = Set Garden

plants :: Stream s m Char => ParsecT s u m Garden
plants = many1 (oneOf ".#")
     >>= return . map fst . filter ((=='#') . snd) . zip [0..]
rule :: Stream s m Char => ParsecT s u m [Garden]
rule = (\ps c -> guard (c == '#') >> return ps)
   <$> (map (subtract 2) <$> plants)
   <*> (string " => " *> oneOf ".#")
parser :: Stream s m Char => ParsecT s u m (Garden, Rules)
parser = (,) <$> (string "initial state: " *> plants <* many1 newline)
             <*> (Set.fromList . msum <$> many (rule <* spaces))

runStep :: RWS Rules [Integer] Garden ()
runStep = do
  garden <- get
  rules <- ask
  let nums = [head garden - 2 .. last garden + 2]
      query = Set.fromList garden
      alive n = flip Set.member rules
              . map fst
              . filter (flip Set.member query . uncurry (+))
              . zip [-2..2] $ repeat n
      garden' = filter alive nums
  modify $ const garden'
  tell . return $ sum garden'

runSteps 0 = return ()
runSteps n = runStep >> runSteps (n-1)

main = do
  (garden, rules) <- either (error . show) id . parse parser "" <$> getContents
  let (_, w) = execRWS (get >>= tell . return . sum >> runSteps 2000) rules garden
  putStrLn "Look for a pattern, here are the first 2000 samples:"
  mapM_ print . zip w . (0:) $ zipWith (-) (tail w) w
