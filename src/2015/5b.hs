{-# LANGUAGE FlexibleContexts #-}

import Data.Either
import Data.Functor
import Text.Parsec
import Text.Parsec.Char

aba = (anyChar <* anyChar) >>= char

aabaa = count 2 anyChar >>= manyTill anyChar . try . string

nice = do
  lookAhead . manyTill anyChar . try $ aba
  lookAhead . manyTill anyChar . try $ aabaa

main = getContents >>= print . length . rights . map (parse nice "") . lines
