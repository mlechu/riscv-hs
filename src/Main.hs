module Main where

import Memory
import Cpu


main :: IO ()
main = do
  let mem = Memory.make [
        (00, 0x9302c003), --  li       t0,0x3c
        (04, 0x13030000), --  mv       t1,zero
        (08, 0x13035300), --  addi     t1,t1,0x5
        (12, 0x9382f2ff), --  addi     t0,t0,-0x1
        (16, 0xe31c53fe), --  bne      t1,t0,LAB_00000008
        (20, 0x63180300), --  bne      t1,zero,LAB_00000024
        (24, 0x13050000), --  mv       a0,zero
        (28, 0x9308d005), --  li       a7,0x5d
        (32, 0x73000000), --  ecall
        (36, 0x1305a002), --  li       a0,0x2a
        (40, 0x9308d005), --  li       a7,0x5d
        (44, 0x73000000)  --  ecall
        ]
      cpu = initCpu
  printExecution cpu mem
