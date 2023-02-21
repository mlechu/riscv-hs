module Memory where

import Data.Bits
import Data.Int
import qualified Data.Map as Map

-- TODO: little-endian
-- TODO: alignment?
-- TODO: probably need to rewrite if upgrading to 64bit

type Address = Int32

type MemWord = Int32

type Memory = Map.Map Address MemWord

make :: [(Address, MemWord)] -> Memory
make = Map.fromList

read32 :: Memory -> Address -> Int32
read32 m a = Map.findWithDefault 0 a m

read8 :: Memory -> Address -> Int8
read8 m a =
  let offset = mod a 4
      word = read32 m (a - offset)
      sh = fromIntegral $ 8 * (4 - 1 - offset)
   in fromIntegral $ shiftR word sh .&. 0xff

write32 :: Memory -> Address -> Int32 -> Memory
write32 m a v
  | mod a 4 /= 0 = error "not aligned"
  | otherwise = Map.insert a v m
