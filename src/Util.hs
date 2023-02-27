module Util where

import Data.Bits
import Data.Int
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

-- A single bit at position n
bitI :: (Integral a, Bits a, Integral b) => Int -> a -> b
bitI n = bitsI n n

-- Bytewise reverse depending on the size of x
-- intended for converting endianness
byteReverse :: (FiniteBits a, Integral a) => a -> a
byteReverse x =
  -- foldr (\b out -> out .|. shiftL (shiftR x b .&. 0xff) (bytes - 1 - b)) 0 [0..(bytes - 1)]
  foldr (\(r, l) out -> out .|. shiftL (shiftR x r .&. 0xff) l) 0 shs
  where
    msb = (finiteBitSize x `div` 8) - 1
    shrs = [0,8..msb*8]
    shs = zip shrs [msb*8,msb*8-8..0]

-- interpret n as a b-bit signed integer
-- b is always less than 32
signExt :: (Bits a, Integral a) => Int -> a -> a
signExt b n =
  if testBit n (b - 1)
    then complement (mask b) .|. n
    else n

-- shiftL with more lenient shift amount type
shL :: (Bits a, Integral b) => a -> b -> a
shL x b = shiftL x (fromIntegral b)

-- shiftR chooses to sign-extend depending on type
-- shRLog and shRAri ignore type and force one behaviour
shRLog :: (Bits a, Integral a, Integral b) => a -> b -> a
shRLog x b = shiftR x (fromIntegral b) .&. complement (mask (fromIntegral b))

shRAri :: (FiniteBits a, Integral a, Integral b) => a -> b -> a
shRAri x b =
  shiftR x (fromIntegral b)
    .|. if countLeadingZeros x == 0
      then complement $ mask $ fromIntegral b
      else 0

vecset :: V.Vector a -> Int -> a -> V.Vector a
vecset vec i val = V.update vec (V.singleton (i, val))

signedOp :: (Integral a) => (Int32 -> Int32 -> t) -> a -> a -> t
signedOp f x y = f (toSigned32 x) (toSigned32 y)

toSigned32 :: (Integral a) => a -> Int32
toSigned32 = fromIntegral

toUnsigned32 :: (Integral a) => a -> Word32
toUnsigned32 = fromIntegral

toUnsigned64 :: (Integral a) => a -> Word64
toUnsigned64 = fromIntegral
