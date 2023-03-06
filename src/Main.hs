module Main where

-- import Numeric

import Control.Monad
import Cpu
import Cpu (printExecution)
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import Data.Elf
import Debug.Trace
import Memory
import System.Environment
import Util
import Util (toUnsigned32)

main :: IO ()
main = do
  args <- getArgs
  if null args then runExample else runElf $ head args

-- -- calls the other
-- (44, 0x130101ff), -- addi   sp,sp,-16
-- (48, 0x23261100), -- sw   ra,12(sp)
-- (52, 0x23248100), -- sw   s0,8(sp)
-- (56, 0x13040101), -- addi   s0,sp,16
-- (60, 0x232aa4fe), -- sw   a0,-12(s0)
-- (64, 0x032544ff), -- lw   a0,-12(s0)
-- (68, 0x97000000), -- auip   ra,0x0
-- (72, 0xe7800000), -- jalr   ra # 44 <a+0x18>
-- (76, 0x8320c100), -- lw   ra,12(sp)
-- (80, 0x03248100), -- lw   s0,8(sp)
-- (84, 0x13010101), -- addi   sp,sp,16
-- (88, 0x67800000), -- ret

runExample :: IO ()
runExample = do
  let mem =
        Memory.make
          [ (00, 0x130101ff), -- addi  sp,sp,-16
            (04, 0x23261100), --  sw  ra,12(sp)
            (08, 0x23248100), --  sw  s0,8(sp)
            (12, 0x13040101), --  addi  s0,sp,16
            (16, 0x232aa4fe), --  sw  a0,-12(s0)
            (20, 0x032544ff), --  lw  a0,-12(s0)
            (24, 0x032544ff), --(24, 0x02a50533) --  mul  a0,a0,a0
            (28, 0x8320c100), --  lw  ra,12(sp)
            (32, 0x03248100), --  lw  s0,8(sp)
            (36, 0x13010101) --  addi  sp,sp,16
            -- , (40, 0x67800000) --  ret (i.e. run forever)
          ]
      cpu = initCpu
  printExecution cpu mem

-- Load an ELF file from path and run it
runElf :: FilePath -> IO ()
runElf file = do
  bs <- BS.readFile file
  let elf = parseElf bs
  -- elf <- fmap parseElf (BS.readFile file)
  -- elf <- parseElf <$> (BS.readFile file)

  case elfClass elf of
    ELFCLASS32 -> putStrLn $ file ++ ": 32-bit ELF"
    ELFCLASS64 -> error "64-bit not supported"

  case elfData elf of
    ELFDATA2LSB -> pure ()
    ELFDATA2MSB -> error "big-endian not yet supported"

  case elfMachine elf of
    (EM_EXT 0xF3) -> pure () -- risc-v only
    _ -> error "no"

  let segments = elfSegments elf
      mem = foldl loadSegment (Memory.make []) segments
      entry = toUnsigned32 $ elfEntry elf
  -- mapM_ (print . show) segments
  -- print mem

  printExecution initCpu {pc = entry} mem

-- Load a single ELF segment into memory at its virtual address
loadSegment :: Memory -> ElfSegment -> Memory
loadSegment m es@ElfSegment {elfSegmentType = PT_LOAD} =
  let d = elfSegmentData es
      base = elfSegmentVirtAddr es
      addrs = map toUnsigned32 $ map (base +) [0 .. elfSegmentMemSize es]
      mdata = BS.unpack $ elfSegmentData es
   in foldl (\m (adr, dat) -> Memory.write8 m adr dat) m (zip addrs mdata)
loadSegment m _ = m
