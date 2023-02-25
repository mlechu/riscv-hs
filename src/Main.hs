module Main where

import Cpu
import Memory

main :: IO ()
main = do
  let mem =
        Memory.make
          [ (00, 0xff010113) -- addi  sp,sp,-16
          , (04, 0x00112623) --  sw  ra,12(sp)
          , (08, 0x00812423) --  sw  s0,8(sp)
          , (12, 0x01010413) --  addi  s0,sp,16
          , (16, 0xfea42a23) --  sw  a0,-12(s0)
          , (20, 0xff442503) --  lw  a0,-12(s0)
          , (24, 0xff442503) --(24, 0x02a50533) --  mul  a0,a0,a0
          , (28, 0x00c12083) --  lw  ra,12(sp)
          , (32, 0x00812403) --  lw  s0,8(sp)
          , (36, 0x01010113) --  addi  sp,sp,16
          -- , (40, 0x00008067) --  ret
          ]
      cpu = initCpu
  printExecution cpu mem

-- -- calls the other
-- (44, ff010113), -- addi   sp,sp,-16
-- (48, 00112623), -- sw   ra,12(sp)
-- (52, 00812423), -- sw   s0,8(sp)
-- (56, 01010413), -- addi   s0,sp,16
-- (60, fea42a23), -- sw   a0,-12(s0)
-- (64, ff442503), -- lw   a0,-12(s0)
-- (68, 00000097), -- auip   ra,0x0
-- (72, 000080e7), -- jalr   ra # 44 <a+0x18>
-- (76, 00c12083), -- lw   ra,12(sp)
-- (80, 00812403), -- lw   s0,8(sp)
-- (84, 01010113), -- addi   sp,sp,16
-- (88, 00008067), -- ret
