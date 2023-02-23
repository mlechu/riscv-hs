module Instruction where

import Data.Int
import Data.Bits
import Util

type Regcode = Int32

-- data Instruction = Instruction Opcode InsBody
  -- = InsTypeR -- register-register
  -- | InsTypeI -- register-immediate, loads, JALR, SYSTEM
  -- | InsTypeS -- stores
  -- | InsTypeU -- LUI, AUIPC
  -- | InsTypeB -- conditional branch
  -- | InsTypeJ -- unconditional jump except JALR (I)

data Instruction
  = OpLUI { rd::Regcode, imm::Int32 }
  | OpAUIPC { rd::Regcode, imm::Int32 }
  | OpJAL { rd::Regcode, imm::Int32 }
  | OpJALR { fn3::Int32, rd::Regcode, rs1::Regcode, imm::Int32 }
  | OpBRANCH { fn3::Int32, rs1::Regcode, rs2::Regcode, imm::Int32 }
  | OpLOAD { fn3::Int32, rd::Regcode, rs1::Regcode, imm::Int32 }
  | OpSTORE { fn3::Int32, rs1::Regcode, rs2::Regcode, imm::Int32 }
  | OpOP_IMM { fn3::Int32, rd::Regcode, rs1::Regcode, imm::Int32 }
  | OpOP_RR { fn7::Int32, fn3::Int32, rd::Regcode, rs1::Regcode, rs2::Regcode }
  | OpMISC_MEM { fm::Int32, fn3::Int32, rd::Regcode, rs1::Regcode,
                          pred::Int32, succ::Int32 }
  | OpSYSTEM { fn12::Int32, fn3::Int32, rd::Regcode, rs1::Regcode }


getRd :: (Num a, Bits a) => a -> a
getRd = bitsI 11 7

getRs1 :: (Num a, Bits a) => a -> a
getRs1 = bitsI 19 15

getRs2 :: (Num a, Bits a) => a -> a
getRs2 = bitsI 24 20

getIImm :: (Bits a, Num a) => a -> a
getIImm i =
  sign 12
  $ bitsI 31 20 i

getSImm :: (Bits a, Num a) => a -> a
getSImm i =
  sign 12
  $ shiftL (bitsI 31 25 i) 5
  .|. bitsI 11 7 i

getBImm :: (Bits a, Num a) => a -> a
getBImm i =
  sign 13
  $ shiftL (bitI 31 i) 12
  .|. shiftL (bitI 7 i) 11
  .|. shiftL (bitsI 30 25 i) 5
  .|. shiftL (bitsI 11 8 i) 1

getUImm :: (Bits a, Num a) => a -> a
getUImm i =
  sign 32
  $ shiftL (bitsI 31 12 i) 12

getJImm :: (Bits a, Num a) => a -> a
getJImm i =
  sign 21
  $ shiftL (bitI 31 i) 20
  .|.shiftL (bitsI 19 12 i) 12
  .|.shiftL (bitI 20 i) 11
  .|.shiftL (bitsI 30 25 i) 5
  .|.shiftL (bitsI 24 21 i) 1

getFn3 :: (Bits a, Num a) => a -> a
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
