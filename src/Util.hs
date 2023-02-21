module Util where

import Data.Bits

-- e.g. [32:0] for bits 31 through 0
getBits :: (Num a, Bits a) => a -> Int -> Int -> a
getBits a start end =
  shiftR a end .&. mask
  where
    mask = shiftL 1 (start - end) - 1

-- inclusive bounds
getBitsI :: (Num a, Bits a) => a -> Int -> Int -> a
getBitsI a start = getBits a (start + 1)
