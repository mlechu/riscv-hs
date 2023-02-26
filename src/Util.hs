module Util where

import Data.Bits
import qualified Data.Vector as V
import Data.Word

mask :: (Num a, Bits a) => Int -> a
mask n = shiftL 1 n - 1

-- e.g. [32:0] for bits 31 through 0
bits :: (Integral a, Bits a, Integral b) => Int -> Int -> a -> b
bits start end a =
  if start <= end
    then error "bad args to bits"
    else fromIntegral $ shiftR a end .&. mask (start - end)

-- inclusive bounds
-- for now, [0:3] = [3:0] (getting the order wrong produces unhelpful errors)
bitsI :: (Integral a, Bits a, Integral b) => Int -> Int -> a -> b
bitsI start = bits (start + 1)

bitI :: (Integral a, Bits a, Integral b) => Int -> a -> b
bitI n = bitsI n n

-- interpret n as a b-bit signed integer
-- b is always less than 32
signExt :: (Bits a, Integral a) => Int -> a -> a
signExt b n =
   if testBit n (b - 1)
      then complement (mask b) .|. n
      else n

-- vecset :: (V.Vector v a) => v a -> Int -> a -> v a
vecset vec i val = V.update vec (V.singleton (i, val))

--
toUnsigned32 :: (Integral a) => a -> Word32
toUnsigned32 = fromIntegral

toUnsigned64 :: (Integral a) => a -> Word64
toUnsigned64 = fromIntegral
