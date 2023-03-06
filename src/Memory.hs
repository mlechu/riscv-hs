module Memory where

import Data.Bits
import Data.Int
import qualified Data.Map as Map
import Data.Word
import Numeric
import Util

type Address = Word32

type MemWord = Word32

data Memory = Memory {mdata :: Map.Map Address MemWord}

instance Show Memory where
  show m =
    Map.foldlWithKey'
      ( \out addr val ->
          out ++ "\n"
            ++ showHex addr ""
            ++ ": "
            -- pad hex digits with blank space
            ++ replicate (8 - length (showHex val "")) ' '
            ++ showHex val ""
      )
      ""
      (mdata m)

make :: [(Address, MemWord)] -> Memory
make l = Memory $ Map.fromList l

-- Assumes a 4-aligned address
read32 :: Memory -> Address -> MemWord
read32 (Memory md) a = byteReverse $ Map.findWithDefault 0 a md

-- Read one byte from memory
read8 :: Memory -> Address -> Word8
read8 (Memory md) a =
  let offset = mod a 4
      word = Map.findWithDefault 0 (a - offset) md
      sh = fromIntegral $ 8 * (4 - 1 - offset)
   in fromIntegral $ shiftR word sh .&. 0xff

-- Generic read function for any number of bytes
readUnaligned :: (Integral b) => Memory -> Address -> b -> Integer
readUnaligned m a bytes =
  foldl
    (\out b -> shiftL out 8 .|. b)
    0
    $ map (fromIntegral . read8 m . (+ a) . fromIntegral) [bytes -1, bytes -2 .. 0]

-- Assumes a 4-aligned address
write32 :: Memory -> Address -> MemWord -> Memory
write32 (Memory md) a v
  | mod a 4 /= 0 = error "not aligned"
  | otherwise = Memory $ Map.insert a (byteReverse v) md

write8 :: Memory -> Address -> Word8 -> Memory
write8 (Memory md) a v =
  let offset = mod a 4
      base = a - offset
      word = Map.findWithDefault 0 base md
      mask = complement $ shiftL 0xff $ fromIntegral $ 8 * (4 - 1 - offset)
      bbb = word .&. mask
      b = shiftL (fromIntegral v) $ fromIntegral $ 8 * (4 - 1 - offset)
   in Memory $ Map.insert base (b .|. bbb) md

-- Write any number of bytes in any position
writeUnaligned :: (Integral b) => Memory -> Address -> b -> Integer -> Memory
writeUnaligned m a b v =
  let bvals = map (\bi -> fromIntegral $ shiftR v (8 * bi) .&. 0xff) [0 .. fromIntegral b -1]
      addrs = [a .. a + fromIntegral b - 1]
   in foldl (\m' (addr, val) -> write8 m' addr val) m (zip addrs bvals)
