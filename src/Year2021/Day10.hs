{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Year2021.Day10 where

import Safe
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List
import Data.Either
import Data.Ord
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Bimap as Bimap
import Control.Monad
import System.FilePath

part1, part2 :: [Either Char String] -> Int
part1 = sum . map (`lookupJust` table) . lefts
  where table = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
part2 = fst . head . filter (uncurry (==)) . (zip <*> reverse) . sort . map score . rights
  where table = [('(', 1), ('[', 2), ('{', 3), ('<', 4)]
        score = foldl (\s x -> s * 5 + x) 0 . map (`lookupJust` table)

parser :: Parsec (Int, Char) String ()
parser = void . many . choice . map match $ words "{} [] () <>" where
  match [x, y] = do
    o <- getOffset
    single x
    observing (parser *> single y) >>= \case
      Left err -> registerFancyFailure (Set.singleton (ErrorCustom (o, x))) *> parseError err
      Right _ -> return ()

foldErrors :: Either (ParseErrorBundle String (Int, Char)) a -> Either Char String
foldErrors (Left ParseErrorBundle{..}) = case token of
  Just (Tokens (x NE.:| _)) -> Left x
  Just EndOfInput -> Right . map snd $ sortOn Down
    [e | FancyError _ xs <- expected, ErrorCustom e <- Set.toList xs]
  where TrivialError _ token _ NE.:| expected = bundleErrors

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part2 . map (foldErrors . parse (parser <* eof) "") . lines

