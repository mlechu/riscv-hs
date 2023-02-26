module Main where

import Cpu
import Data.List.Split
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as DL
import Memory
import System.Exit
import System.Directory

main = do
  testLoop

testLoop = do
  let mem =
        Memory.make
          [ (00, 0xff010113), -- addi  sp,sp,-16
            (04, 0x00112623), --  sw  ra,12(sp)
            (08, 0x00812423), --  sw  s0,8(sp)
            (12, 0x01010413), --  addi  s0,sp,16
            (16, 0xfea42a23), --  sw  a0,-12(s0)
            (20, 0xff442503), --  lw  a0,-12(s0)
            (24, 0xff442503), -- (24, 0x02a50533) --  mul  a0,a0,a0
            (28, 0x00c12083), --  lw  ra,12(sp)
            (32, 0x00812403), --  lw  s0,8(sp)
            (36, 0x01010113) --  addi  sp,sp,16
            -- , (40, 0x00008067) --  ret
          ]
      cpu = initCpu
  mem2 <- loadTest "test/bins/square.bin"
  printExecution cpu mem2

loadTest :: FilePath -> IO Memory
loadTest test = do
  bin <- DL.readFile test
  return (Memory.make (Prelude.zip [0 .. 24] (Prelude.map word32FromChars (chunkBs 4 bin))))

chunkBs n bs
  | DL.null bs = []
  | otherwise = DL.take n bs : chunkBs n (DL.drop n bs)

word32FromChars = B.runGet B.getWord32le
