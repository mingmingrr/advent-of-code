{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Year2021.Day10 where

import Safe
import Text.Megaparsec
import Data.List
import Data.Either
import System.FilePath
import Data.Generics

part1, part2 :: [Either Char String] -> Int
part1 = sum . map (`lookupJust` table) . lefts
  where table = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
part2 = fst . head . filter (uncurry (==)) . (zip <*> reverse) . sort . map score . rights
  where table = [('(', 1), ('[', 2), ('{', 3), ('<', 4)]
        score = foldl (\s x -> s * 5 + x) 0 . map (`lookupJust` table)

parser :: Parsec (Int, Char) String ()
parser = (() <$) . many . choice . map match $ words "{} [] () <>" where
  match [x, y] = do
    o <- getOffset
    single x
    observing (parser *> single y) >>= \case
      Left err -> registerParseError err *> customFailure (o, x)
      Right _ -> return ()

foldErrors :: Either (ParseErrorBundle String (Int, Char)) () -> Either Char String
foldErrors errs = case something (mkQ Nothing queryToken) errs of
  Just (Tokens xs) -> Left . head $ listify (const True) xs
  Just EndOfInput -> Right . reverse . map snd . sort $
    listify (\(_ :: (Int, Char)) -> True) errs
  where queryToken :: ParseError String (Int, Char) -> Maybe (ErrorItem Char)
        queryToken = \case TrivialError _ (Just token) _ -> Just token ; _ -> Nothing

main = readFile (replaceExtension __FILE__ ".in") >>=
  print . part2 . map (foldErrors . parse (parser <* eof) "") . lines

