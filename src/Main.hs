module Main where

import Cpu
import Memory
import Debug.Trace
import Numeric
import Util

main :: IO ()
main = do
  let mem =
        Memory.make
          [ (00, 0x130101ff) -- addi  sp,sp,-16
          , (04, 0x23261100) --  sw  ra,12(sp)
          , (08, 0x23248100) --  sw  s0,8(sp)
          , (12, 0x13040101) --  addi  s0,sp,16
          , (16, 0x232aa4fe) --  sw  a0,-12(s0)
          , (20, 0x032544ff) --  lw  a0,-12(s0)
          , (24, 0x032544ff) --(24, 0x02a50533) --  mul  a0,a0,a0
          , (28, 0x8320c100) --  lw  ra,12(sp)
          , (32, 0x03248100) --  lw  s0,8(sp)
          , (36, 0x13010101) --  addi  sp,sp,16
          -- , (40, 0x67800000) --  ret
          ]
      cpu = initCpu
  printExecution cpu mem

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
