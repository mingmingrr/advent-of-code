module Numeric.Char where

import Data.Char

instance Num Char where
  x + y = chr (ord x + ord y)
  x - y = chr (ord x - ord y)
  (*) = error "char: undefined (*)"
  abs = error "char: undefined abs"
  signum = error "char: undefined signum"
  fromInteger = intToDigit . fromInteger

instance Real Char where
  toRational = toRational . digitToInt

instance Integral Char where
  quotRem = error "char: undefined quotRem"
  toInteger = toInteger . digitToInt

