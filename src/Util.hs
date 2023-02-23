module Util where

import Data.Bits

mask :: (Num a, Bits a) => Int -> a
mask n = shiftL 1 n - 1

-- e.g. [32:0] for bits 31 through 0
bits :: (Num a, Bits a) => Int -> Int -> a -> a
bits start end a =
  shiftR a end .&. mask (start - end)

-- inclusive bounds
bitsI :: (Num a, Bits a) => Int -> Int -> a -> a
bitsI start = bits (start + 1)

bitI :: (Num a, Bits a) => Int -> a -> a
bitI n = bitsI n n

-- interpret n as a b-bit signed integer
-- b is always less than 32
sign :: (Bits a, Num a) => Int -> a -> a
sign b n =
  if testBit n (b - 1)
  then negate $ mask b .&. (complement n) + 1
  else n
