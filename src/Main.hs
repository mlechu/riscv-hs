module Main where

import Control.Monad
import Cpu
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import Data.Elf
import Memory
import System.Environment
import Util

main :: IO ()
main = do
  args <- getArgs
  if null args then runExample else runElf $ head args

-- Run an example program (program counter defaults to 0x0)
runExample :: IO ()
runExample = do
  print "No file provided: running example program"
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
          ]
      cpu = initCpu
  printExecution cpu mem

-- Load segments and entry point from ELF filepath and print execution
-- Will raise an error if the file is not an ELF, or is of an unsupported type
runElf :: FilePath -> IO ()
runElf file = do
  bs <- BS.readFile file
  let elf = parseElf bs
  -- elf <- fmap parseElf (BS.readFile file)
  -- elf <- parseElf <$> (BS.readFile file)

  case elfClass elf of
    ELFCLASS32 -> putStrLn $ file ++ ": loading 32-bit ELF"
    ELFCLASS64 -> error "64-bit not yet supported"

  case elfData elf of
    ELFDATA2LSB -> pure ()
    ELFDATA2MSB -> error "Big-endian not yet supported"

  -- risc-v only
  case elfMachine elf of
    (EM_EXT 0xF3) -> pure ()
    _ -> error "no"

  let segments = elfSegments elf
      mem = foldl loadSegment (Memory.make []) segments
      entry = toUnsigned32 $ elfEntry elf

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
