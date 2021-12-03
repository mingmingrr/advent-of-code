module Numeric.Char where

instance Num Char where
  (+) = error "char: undefined (+)"
  (*) = error "char: undefined (*)"
  (-) = error "char: undefined (-)"
  abs = error "char: undefined abs"
  signum = error "char: undefined signum"
  fromInteger = intToDigit . fromInteger

instance Real Char where
  toRational = toRational . digitToInt

instance Integral Char where
  quotRem = error "char: undefined quotRem"
  toInteger = toInteger . digitToInt

