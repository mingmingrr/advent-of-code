module Numeric.Bool where

instance Num Bool where
  (+) = (||)
  (*) = (&&)
  abs = id
  signum = id
  fromInteger = toEnum . fromInteger
  negate = not

instance Real Bool where
  toRational = toRational . fromEnum

instance Integral Bool where
  quotRem x y = (toEnum q, toEnum r)
    where (q, r) = quotRem (fromEnum x) (fromEnum y)
  toInteger = toInteger . fromEnum

