module Instruction where

import Data.Int
import Data.Word
import Data.Bits
import Util

type Regcode = Int

data Instruction
  = OpLUI { rd::Regcode, imm::Word32 }
  | OpAUIPC { rd::Regcode, imm::Word32 }
  | OpJAL { rd::Regcode, imm::Word32 }
  | OpJALR { fn3::Int32, rd::Regcode, rs1::Regcode, imm::Word32 }
  | OpBRANCH { fn3::Int32, rs1::Regcode, rs2::Regcode, imm::Word32 }
  | OpLOAD { fn3::Int32, rd::Regcode, rs1::Regcode, imm::Word32 }
  | OpSTORE { fn3::Int32, rs1::Regcode, rs2::Regcode, imm::Word32 }
  | OpOP_IMM { fn3::Int32, rd::Regcode, rs1::Regcode, imm::Word32 }
  | OpOP_RR { fn7::Int32, fn3::Int32, rd::Regcode, rs1::Regcode, rs2::Regcode }
  | OpMISC_MEM { fm::Int32, fn3::Int32, rd::Regcode, rs1::Regcode,
                          pred4::Int32, succ4::Int32 }
  | OpSYSTEM { fn12::Int32, fn3::Int32, rd::Regcode, rs1::Regcode }
  | OpUNIMP { ins::Word32 }
  deriving (Show)


-- data InstructionComponent
  -- = InsRd | InsRs1 | InsRs2 | InsImmI | InsImmS | InsImmB | InsImmU | InsImmJ | InsFn3

getRd :: (Integral a, Bits a, Integral b, Bits b) => a -> b
getRd = bitsI 11 7

getRs1 :: (Integral a, Bits a, Integral b, Bits b) => a -> b
getRs1 = bitsI 19 15

getRs2 :: (Integral a, Bits a, Integral b, Bits b) => a -> b
getRs2 = bitsI 24 20

getIImm :: (Integral a, Bits a, Integral b, Bits b) => a -> b
getIImm i =
  signExt 12
  $ bitsI 31 20 i

getSImm :: (Integral a, Bits a, Integral b, Bits b) => a -> b
getSImm i =
  signExt 12
  $ shiftL (bitsI 31 25 i) 5
  .|. bitsI 11 7 i

getBImm :: (Integral a, Bits a, Integral b, Bits b) => a -> b
getBImm i =
  signExt 13
  $ shiftL (bitI 31 i) 12
  .|. shiftL (bitI 7 i) 11
  .|. shiftL (bitsI 30 25 i) 5
  .|. shiftL (bitsI 11 8 i) 1

getUImm :: (Integral a, Bits a, Integral b, Bits b) => a -> b
getUImm i =
  signExt 32
  $ shiftL (bitsI 31 12 i) 12

getJImm :: (Integral a, Bits a, Integral b, Bits b) => a -> b
getJImm i =
  signExt 21
  $ shiftL (bitI 31 i) 20
  .|.shiftL (bitsI 19 12 i) 12
  .|.shiftL (bitI 20 i) 11
  .|.shiftL (bitsI 30 25 i) 5
  .|.shiftL (bitsI 24 21 i) 1

getFn3 :: (Integral a, Bits a, Integral b, Bits b) => a -> b
getFn3 = bitsI 14 12


-- OCRed from the pdf lol
-- 0110111 LUI

-- 0010111 AUIPC

-- 1101111 JAL

-- 1100111 JALR

-- 1100011 BEQ
-- 1100011 BNE
-- 1100011 BLT
-- 1100011 BGE
-- 1100011 BLTU
-- 1100011 BGEU

-- 0000011 LB
-- 0000011 LH
-- 0000011 LW
-- 0000011 LBU
-- 0000011 LHU

-- 0100011 SB
-- 0100011 SH
-- 0100011 SW

-- 0010011 ADDI
-- 0010011 SLTI
-- 0010011 SLTIU
-- 0010011 XORI
-- 0010011 ORI
-- 0010011 ANDI
-- 0010011 SLLI
-- 0010011 SRLI
-- 0010011 SRAI

-- 0110011 ADD
-- 0110011 SUB
-- 0110011 SLL
-- 0110011 SLT
-- 0110011 SLTU
-- 0110011 XOR
-- 0110011 SRL
-- 0110011 SRA
-- 0110011 OR
-- 0110011 AND

-- 0001111 FENCE

-- 1110011 ECALL
-- 1110011 EBREAK



-- -- rv64
-- -- shared with 32:
-- 0000011 LWU
-- 0000011 LD

-- 0100011 SD

-- 0010011 SLLI
-- 0010011 SRLI
-- 0010011 SRAI

-- -- new:
-- 0011011 ADDIW
-- 0011011 SLLIW
-- 0011011 SRLIW
-- 0011011 SRAIW

-- 0111011 ADDW
-- 0111011 SUBW
-- 0111011 SLLW
-- 0111011 SRLW
-- 0111011 SRAW
